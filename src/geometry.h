// Geometry helpers for depth sorting
//
// Screen-space tests operate on NPC coordinates; 3D tests on standardized
// cube-space coordinates. Each function mirrors the R implementation it
// replaced, including tolerances.

#ifndef GGCUBE_GEOMETRY_H
#define GGCUBE_GEOMETRY_H

#include <vector>
#include <cmath>
#include <algorithm>
#include <limits>

namespace ggcube {

const double NA_DBL = std::numeric_limits<double>::quiet_NaN();

struct Vec3 {
      double x, y, z;
};

inline double dot3(const Vec3& a, const Vec3& b) { return a.x * b.x + a.y * b.y + a.z * b.z; }
inline Vec3 sub3(const Vec3& a, const Vec3& b) { return {a.x - b.x, a.y - b.y, a.z - b.z}; }

// A polygon's best-fit plane (Newell's method): normal . v = offset, with
// `warp` the largest vertex distance from it
struct Plane {
      bool valid = false;
      Vec3 normal{0, 0, 0};
      double offset = 0;
      double warp = 0;
      bool planar = false;
      double area = 0;
};

// Sum of three products, accumulated in extended precision as R's sum() does
inline double sum3(double a1, double b1, double a2, double b2, double a3, double b3) {
      long double s = 0;
      s += (double) (a1 * b1);
      s += (double) (a2 * b2);
      s += (double) (a3 * b3);
      return (double) s;
}

inline double dot3_sum(const Vec3& a, const Vec3& b) { return sum3(a.x, b.x, a.y, b.y, a.z, b.z); }

// Best-fit plane, with sums accumulated as in the R implementation (sum() and
// colMeans() in extended precision, matrix products in double)
inline Plane polygon_plane(const std::vector<Vec3>& p, double tol = 1e-6) {
      Plane pl;
      size_t n = p.size();
      if (n == 0) return pl;
      long double nx = 0, ny = 0, nz = 0, cx = 0, cy = 0, cz = 0;
      for (size_t i = 0; i < n; i++) {
            const Vec3& a = p[i];
            const Vec3& b = p[(i + 1) % n];
            nx += (double) ((a.y - b.y) * (a.z + b.z));
            ny += (double) ((a.z - b.z) * (a.x + b.x));
            nz += (double) ((a.x - b.x) * (a.y + b.y));
            cx += a.x; cy += a.y; cz += a.z;
      }
      double vx = (double) nx, vy = (double) ny, vz = (double) nz;
      long double ss = 0;
      ss += (double) (vx * vx); ss += (double) (vy * vy); ss += (double) (vz * vz);
      double len = std::sqrt((double) ss);
      if (len < 1e-12) return pl;
      pl.normal = {vx / len, vy / len, vz / len};
      Vec3 c{(double) (cx / n), (double) (cy / n), (double) (cz / n)};
      pl.offset = dot3_sum(c, pl.normal);
      double w = 0;
      for (size_t i = 0; i < n; i++) w = std::max(w, std::fabs(dot3(p[i], pl.normal) - pl.offset));
      pl.warp = w;
      pl.planar = w <= tol;
      pl.area = len / 2;
      pl.valid = true;
      return pl;
}

// Even-odd point-in-polygon by ray casting (as point_in_polygon() in R)
inline bool point_in_polygon(double px, double py, const double* x, const double* y, int n) {
      bool inside = false;
      int j = n - 1;
      for (int i = 0; i < n; i++) {
            if (((y[i] > py) != (y[j] > py)) &&
                (px < (x[j] - x[i]) * (py - y[i]) / (y[j] - y[i]) + x[i])) {
                  inside = !inside;
            }
            j = i;
      }
      return inside;
}

// Separating axis test on two screen polygons, using edge normals of both
// (as sat_overlap() in R). Edges of zero length are skipped.
inline bool sat_overlap(const double* x1, const double* y1, int n1,
                        const double* x2, const double* y2, int n2, double tol = 1e-9) {
      for (int v = 0; v < 2; v++) {
            const double* ax = v == 0 ? x1 : x2;
            const double* ay = v == 0 ? y1 : y2;
            int an = v == 0 ? n1 : n2;
            const double* bx = v == 0 ? x2 : x1;
            const double* by = v == 0 ? y2 : y1;
            int bn = v == 0 ? n2 : n1;
            for (int i = 0; i < an; i++) {
                  int k = (i + 1) % an;
                  double ex = ax[k] - ax[i], ey = ay[k] - ay[i];
                  double nrm = std::sqrt(ey * ey + ex * ex);
                  double nx = -ey / nrm, ny = ex / nrm;
                  if (!std::isfinite(nx) || !std::isfinite(ny)) continue;
                  double amin = INFINITY, amax = -INFINITY, bmin = INFINITY, bmax = -INFINITY;
                  for (int m = 0; m < an; m++) {
                        double p = ax[m] * nx + ay[m] * ny;
                        amin = std::min(amin, p); amax = std::max(amax, p);
                  }
                  for (int m = 0; m < bn; m++) {
                        double p = bx[m] * nx + by[m] * ny;
                        bmin = std::min(bmin, p); bmax = std::max(bmax, p);
                  }
                  if (amax <= bmin + tol || bmax <= amin + tol) return false;
            }
      }
      return true;
}

// Signed area and centroid of a ring (as get_centroid() in R). Returns
// false for a ring of zero area.
inline bool ring_centroid(const std::vector<double>& x, const std::vector<double>& y,
                          double& cx, double& cy, double& area) {
      size_t n = x.size();
      if (n < 3) return false;
      double a = 0, sx = 0, sy = 0;
      for (size_t i = 0; i < n; i++) {
            size_t k = (i + 1) % n;
            double c = x[i] * y[k] - x[k] * y[i];
            a += c;
            sx += (x[i] + x[k]) * c;
            sy += (y[i] + y[k]) * c;
      }
      area = a / 2;
      if (area == 0 || !std::isfinite(area)) return false;
      cx = sx / (6 * area);
      cy = sy / (6 * area);
      return true;
}

inline double ring_area(const double* x, const double* y, int n) {
      double a = 0;
      for (int i = 0; i < n; i++) {
            int k = (i + 1) % n;
            a += x[i] * y[k] - x[k] * y[i];
      }
      return a / 2;
}

// Whether a ring is convex (collinear and repeated vertices allowed)
inline bool ring_convex(const double* x, const double* y, int n) {
      if (n < 3) return false;
      int sign = 0;
      for (int i = 0; i < n; i++) {
            int j = (i + 1) % n, k = (i + 2) % n;
            double cr = (x[j] - x[i]) * (y[k] - y[j]) - (y[j] - y[i]) * (x[k] - x[j]);
            double scale = std::fabs(x[j] - x[i]) + std::fabs(y[j] - y[i]) +
                  std::fabs(x[k] - x[j]) + std::fabs(y[k] - y[j]);
            if (std::fabs(cr) <= 1e-14 * scale * scale) continue;
            int s = cr > 0 ? 1 : -1;
            if (sign == 0) sign = s;
            else if (s != sign) return false;
      }
      return sign != 0;
}

// Sutherland-Hodgman clip of a subject ring by a convex clip ring. The clip
// ring may be in either orientation; zero-length clip edges are skipped.
inline void clip_convex(const double* sx, const double* sy, int sn,
                        const double* cx, const double* cy, int cn,
                        std::vector<double>& ox, std::vector<double>& oy) {
      ox.assign(sx, sx + sn);
      oy.assign(sy, sy + sn);
      double orient = ring_area(cx, cy, cn) >= 0 ? 1.0 : -1.0;
      std::vector<double> ix, iy;
      for (int e = 0; e < cn && !ox.empty(); e++) {
            int f = (e + 1) % cn;
            double ex = cx[f] - cx[e], ey = cy[f] - cy[e];
            if (ex == 0 && ey == 0) continue;
            auto side = [&](double px, double py) {
                  return orient * (ex * (py - cy[e]) - ey * (px - cx[e]));
            };
            ix.swap(ox); iy.swap(oy);
            ox.clear(); oy.clear();
            size_t m = ix.size();
            for (size_t i = 0; i < m; i++) {
                  size_t k = (i + 1) % m;
                  double si = side(ix[i], iy[i]), sk = side(ix[k], iy[k]);
                  if (si >= 0) { ox.push_back(ix[i]); oy.push_back(iy[i]); }
                  if ((si > 0 && sk < 0) || (si < 0 && sk > 0)) {
                        double t = si / (si - sk);
                        ox.push_back(ix[i] + t * (ix[k] - ix[i]));
                        oy.push_back(iy[i] + t * (iy[k] - iy[i]));
                  }
            }
      }
}

// Intersection point of two screen segments, if they cross within both
// (as segment_intersection() in R)
inline bool segment_intersection(double x1, double y1, double x2, double y2,
                                 double x3, double y3, double x4, double y4,
                                 double& ix, double& iy) {
      double dx1 = x2 - x1, dy1 = y2 - y1;
      double dx2 = x4 - x3, dy2 = y4 - y3;
      double denom = dx1 * dy2 - dy1 * dx2;
      if (std::fabs(denom) < 1e-12) return false;
      double t = ((x3 - x1) * dy2 - (y3 - y1) * dx2) / denom;
      double u = ((x3 - x1) * dy1 - (y3 - y1) * dx1) / denom;
      if (t < 0 || t > 1 || u < 0 || u > 1) return false;
      ix = x1 + t * dx1;
      iy = y1 + t * dy1;
      return true;
}

// Depth along a screen segment at the projection of a point onto it
// (as interpolate_z_on_segment() in R)
inline double interpolate_z_on_segment(double x1, double y1, double z1,
                                       double x2, double y2, double z2,
                                       double px, double py) {
      double dx = x2 - x1, dy = y2 - y1;
      double len_sq = dx * dx + dy * dy;
      if (len_sq < 1e-20) return (z1 + z2) / 2;
      double t = ((px - x1) * dx + (py - y1) * dy) / len_sq;
      t = std::max(0.0, std::min(1.0, t));
      return z1 + t * (z2 - z1);
}

// Portion of a screen segment inside a polygon (even-odd): appends each
// inside piece's end points (as polyclip's open-path intersection, whose
// vertices R averaged). Returns the number of pieces.
inline int segment_inside_pieces(double ax, double ay, double bx, double by,
                                 const double* px, const double* py, int n,
                                 std::vector<double>& vx, std::vector<double>& vy) {
      std::vector<double> ts{0.0, 1.0};
      double dx = bx - ax, dy = by - ay;
      for (int i = 0; i < n; i++) {
            int k = (i + 1) % n;
            double ex = px[k] - px[i], ey = py[k] - py[i];
            double den = dx * ey - dy * ex;
            if (std::fabs(den) < 1e-300) continue;
            double t = ((px[i] - ax) * ey - (py[i] - ay) * ex) / den;
            double u = ((px[i] - ax) * dy - (py[i] - ay) * dx) / den;
            if (t > 0 && t < 1 && u >= 0 && u <= 1) ts.push_back(t);
      }
      std::sort(ts.begin(), ts.end());
      int pieces = 0;
      bool open = false;
      double start = 0;
      for (size_t i = 0; i + 1 < ts.size(); i++) {
            double t0 = ts[i], t1 = ts[i + 1];
            if (t1 - t0 <= 1e-12) continue;
            double tm = (t0 + t1) / 2;
            bool in = point_in_polygon(ax + tm * dx, ay + tm * dy, px, py, n);
            if (in && !open) { open = true; start = t0; }
            if (!in && open) {
                  open = false;
                  vx.push_back(ax + start * dx); vy.push_back(ay + start * dy);
                  vx.push_back(ax + t0 * dx);    vy.push_back(ay + t0 * dy);
                  pieces++;
            }
      }
      if (open) {
            vx.push_back(ax + start * dx); vy.push_back(ay + start * dy);
            vx.push_back(bx);              vy.push_back(by);
            pieces++;
      }
      return pieces;
}


// A point inside a ring (even-odd): the midpoint of the widest interior span
// of the horizontal line through the given height. Returns false if the line
// misses the ring's interior.
inline bool point_on_surface(const std::vector<double>& x, const std::vector<double>& y, double y0,
                             double& px, double& py) {
      size_t n = x.size();
      std::vector<double> xs;
      for (size_t i = 0, j = n - 1; i < n; j = i++) {
            if ((y[i] > y0) != (y[j] > y0)) {
                  xs.push_back(x[i] + (y0 - y[i]) * (x[j] - x[i]) / (y[j] - y[i]));
            }
      }
      std::sort(xs.begin(), xs.end());
      double best = 0;
      bool found = false;
      for (size_t k = 0; k + 1 < xs.size(); k += 2) {
            double w = xs[k + 1] - xs[k];
            if (w > best) {
                  best = w;
                  px = (xs[k] + xs[k + 1]) / 2;
                  py = y0;
                  found = true;
            }
      }
      return found;
}

// A representative point of a ring's interior: its centroid if that lies
// inside, otherwise a point on its surface
inline void interior_point(const std::vector<double>& x, const std::vector<double>& y,
                           double cx, double cy, double& px, double& py) {
      px = cx; py = cy;
      if (point_in_polygon(cx, cy, x.data(), y.data(), (int) x.size())) return;
      double lo = *std::min_element(y.begin(), y.end());
      double hi = *std::max_element(y.begin(), y.end());
      double qx, qy;
      for (double f : {0.5, 0.3, 0.7, 0.1, 0.9}) {
            double y0 = (cy >= lo && cy <= hi && f == 0.5) ? cy : lo + f * (hi - lo);
            if (point_on_surface(x, y, y0, qx, qy)) { px = qx; py = qy; return; }
      }
}

} // namespace ggcube

#endif
