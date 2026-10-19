// Pairwise depth-order constraints between primitives
//
// Candidate pairs are those whose screen bounding boxes overlap (found by a
// sweep over x), excluding pairs that can never constrain each other
// (point/text with point/text, and pieces cut from the same polygon). Each
// candidate is compared with a type-specific screen-space test; where a
// polygon is involved and cube-space geometry is available, the sign of the
// constraint is then settled exactly from 3D geometry where possible.
//
// The comparisons, tolerances, and tie-breaks follow the R implementation
// they replaced. Screen polygon intersections are computed by clipping
// against whichever polygon is convex; pairs of two non-convex polygons
// that need an intersection are returned to R (which uses polyclip) and
// finished by a second call with the intersection centroid supplied.

#include <cpp11.hpp>
#include <vector>
#include <cmath>
#include <algorithm>
#include <numeric>
#include "geometry.h"

using namespace cpp11;
using namespace ggcube;

namespace {

enum PrimType { POLYGON = 0, POINT = 1, SEGMENT = 2, TEXT = 3 };

const double COPLANAR_TOL = 1e-8;
const double SPLIT_TOL = 0.01;

inline double prim_priority(int t) { return t == POLYGON ? 1 : (t == SEGMENT ? 2 : 3); }
inline bool is_pointlike(int t) { return t == POINT || t == TEXT; }

struct Group {
      int start, n, type, cut_parent;
      double xmin, xmax, ymin, ymax, dmin, dmax, dmean;
      bool has_beta = false;
      double beta[3] = {0, 0, 0};
      bool convex = false;
      bool has3d = false;
      std::vector<Vec3> pts;
      Plane plane;
      double touch = 0;
      double radius = 0;      // marker radius (points), in points
      double halfwidth = 0;   // half line width (segments), in points
      int layer = 0;
};

struct Eye {
      bool valid = false;
      bool persp = false;
      Vec3 v{0, 0, 0};
};

struct Proj {
      bool valid = false;
      bool persp = false;
      double dist = 2;
      double rot[9];       // column-major 3x3 rotation matrix
      double pb[4];
};

struct Scene {
      const double *x, *y, *d;
      std::vector<Group> g;
      Eye eye;
      Proj proj;
      // Panel size in points; when known, marker and line extents are used
      bool sized = false;
      double pw = 0, ph = 0;
};

// Distance in points between two screen (npc) points
inline double dist_pt(const Scene& s, double x1, double y1, double x2, double y2) {
      return std::hypot((x1 - x2) * s.pw, (y1 - y2) * s.ph);
}

// Distance in points from a screen point to a screen segment, and the
// closest point on it
inline double dist_to_segment_pt(const Scene& s, double px, double py,
                                 double x1, double y1, double x2, double y2,
                                 double& cx, double& cy) {
      double ax = x1 * s.pw, ay = y1 * s.ph, bx = x2 * s.pw, by = y2 * s.ph;
      double qx = px * s.pw, qy = py * s.ph;
      double dx = bx - ax, dy = by - ay, l2 = dx * dx + dy * dy;
      double t = l2 < 1e-20 ? 0 : std::max(0.0, std::min(1.0, ((qx - ax) * dx + (qy - ay) * dy) / l2));
      cx = x1 + t * (x2 - x1);
      cy = y1 + t * (y2 - y1);
      return std::hypot(qx - (ax + t * dx), qy - (ay + t * dy));
}

// Least-squares plane depth = b0 + b1 x + b2 y over a group's vertices,
// solved from the normal equations; NULL (has_beta false) when the system is
// computationally singular, as with R's solve()
bool fit_beta(const double* x, const double* y, const double* d, int n, double* beta) {
      double a[3][3] = {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}};
      double r[3] = {0, 0, 0};
      for (int i = 0; i < n; i++) {
            double v[3] = {1, x[i], y[i]};
            for (int p = 0; p < 3; p++) {
                  r[p] += v[p] * d[i];
                  for (int q = 0; q < 3; q++) a[p][q] += v[p] * v[q];
            }
      }
      double det = a[0][0] * (a[1][1] * a[2][2] - a[1][2] * a[2][1]) -
            a[0][1] * (a[1][0] * a[2][2] - a[1][2] * a[2][0]) +
            a[0][2] * (a[1][0] * a[2][1] - a[1][1] * a[2][0]);
      if (det == 0 || !std::isfinite(det)) return false;
      double inv[3][3];
      inv[0][0] = (a[1][1] * a[2][2] - a[1][2] * a[2][1]) / det;
      inv[0][1] = (a[0][2] * a[2][1] - a[0][1] * a[2][2]) / det;
      inv[0][2] = (a[0][1] * a[1][2] - a[0][2] * a[1][1]) / det;
      inv[1][0] = (a[1][2] * a[2][0] - a[1][0] * a[2][2]) / det;
      inv[1][1] = (a[0][0] * a[2][2] - a[0][2] * a[2][0]) / det;
      inv[1][2] = (a[0][2] * a[1][0] - a[0][0] * a[1][2]) / det;
      inv[2][0] = (a[1][0] * a[2][1] - a[1][1] * a[2][0]) / det;
      inv[2][1] = (a[0][1] * a[2][0] - a[0][0] * a[2][1]) / det;
      inv[2][2] = (a[0][0] * a[1][1] - a[0][1] * a[1][0]) / det;
      // Reciprocal condition number in the 1-norm
      double n_a = 0, n_inv = 0;
      for (int q = 0; q < 3; q++) {
            double ca = 0, ci = 0;
            for (int p = 0; p < 3; p++) { ca += std::fabs(a[p][q]); ci += std::fabs(inv[p][q]); }
            n_a = std::max(n_a, ca); n_inv = std::max(n_inv, ci);
      }
      double rcond = 1 / (n_a * n_inv);
      if (!(rcond >= std::numeric_limits<double>::epsilon())) return false;
      for (int p = 0; p < 3; p++) beta[p] = inv[p][0] * r[0] + inv[p][1] * r[1] + inv[p][2] * r[2];
      return true;
}

// Depth of a polygon at a screen point: from its plane fit, or by inverse
// distance weighting for degenerate polygons (as interpolate_z() in R)
double interpolate_z(const Scene& s, const Group& g, double px, double py) {
      if (g.has_beta) return g.beta[0] + g.beta[1] * px + g.beta[2] * py;
      double sw = 0, swz = 0;
      for (int k = 0; k < g.n; k++) {
            int r = g.start + k;
            double w = 1 / std::sqrt((s.x[r] - px) * (s.x[r] - px) + (s.y[r] - py) * (s.y[r] - py));
            sw += w; swz += w * s.d[r];
      }
      return swz / sw;
}

// Centroid of the screen intersection of two polygons. Returns 0 if they
// don't intersect, 1 if found, -1 if neither polygon is convex.
int intersection_centroid(const Scene& s, const Group& a, const Group& b, double& cx, double& cy) {
      const Group* clip;
      const Group* subj;
      if (b.convex) { clip = &b; subj = &a; }
      else if (a.convex) { clip = &a; subj = &b; }
      else return -1;
      std::vector<double> ox, oy;
      clip_convex(s.x + subj->start, s.y + subj->start, subj->n,
                  s.x + clip->start, s.y + clip->start, clip->n, ox, oy);
      double area, gx, gy;
      if (!ring_centroid(ox, oy, gx, gy, area)) return 0;
      double ref = std::fabs(ring_area(s.x + clip->start, s.y + clip->start, clip->n));
      if (std::fabs(area) <= 1e-12 * ref) return 0;
      // A non-convex subject can leave the intersection in several parts,
      // whose centroid may fall outside all of them
      interior_point(ox, oy, gx, gy, cx, cy);
      return 1;
}

// Midpoint of the longest of the pieces (pairs of end points) of a segment
// inside a polygon
void longest_piece_midpoint(const std::vector<double>& vx, const std::vector<double>& vy,
                            double& mx, double& my) {
      double best = -1;
      for (size_t k = 0; k + 1 < vx.size(); k += 2) {
            double len = std::hypot(vx[k + 1] - vx[k], vy[k + 1] - vy[k]);
            if (len > best) {
                  best = len;
                  mx = (vx[k] + vx[k + 1]) / 2;
                  my = (vy[k] + vy[k + 1]) / 2;
            }
      }
}

// Midpoint of the portion of a segment overlapping a polygon on screen (of
// its longest part, if the polygon is non-convex and cuts it into several)
bool segment_overlap_midpoint(const Scene& s, const Group& poly, const Group& seg,
                              double& mx, double& my) {
      int r = seg.start;
      std::vector<double> vx, vy;
      int pieces = segment_inside_pieces(s.x[r], s.y[r], s.x[r + 1], s.y[r + 1],
                                         s.x + poly.start, s.y + poly.start, poly.n, vx, vy);
      if (pieces == 0) return false;
      longest_piece_midpoint(vx, vy, mx, my);
      return true;
}

// Screen-space comparisons -------------------------------------------------------
// Each returns a depth delta (positive: i behind j) or NaN for no constraint.

double compare_poly_poly(const Scene& s, const Group& i, const Group& j,
                         const double* given_xy, bool& deferred, double* xy_out, bool& has_xy) {
      if (i.xmin >= j.xmax || i.xmax <= j.xmin || i.ymin >= j.ymax || i.ymax <= j.ymin) return NA_DBL;
      if (!sat_overlap(s.x + i.start, s.y + i.start, i.n, s.x + j.start, s.y + j.start, j.n)) return NA_DBL;
      if (i.dmin >= j.dmax || j.dmin >= i.dmax) {
            return (i.dmin + i.dmax) / 2 - (j.dmin + j.dmax) / 2;
      }
      double cx, cy;
      if (given_xy) {
            if (!std::isfinite(given_xy[0])) return NA_DBL;
            cx = given_xy[0]; cy = given_xy[1];
      } else {
            int found = intersection_centroid(s, i, j, cx, cy);
            if (found < 0) { deferred = true; return NA_DBL; }
            if (found == 0) return NA_DBL;
      }
      xy_out[0] = cx; xy_out[1] = cy; has_xy = true;
      return interpolate_z(s, i, cx, cy) - interpolate_z(s, j, cx, cy);
}

double compare_poly_point(const Scene& s, const Group& poly, const Group& pt) {
      double px = s.x[pt.start], py = s.y[pt.start], pz = s.d[pt.start];
      if (s.sized && pt.radius > 0) {
            // The marker overlaps the polygon if its centre is inside it or
            // its disc reaches one of its edges
            double rx = pt.radius / s.pw, ry = pt.radius / s.ph;
            if (px + rx < poly.xmin || px - rx > poly.xmax || py + ry < poly.ymin || py - ry > poly.ymax) return NA_DBL;
            bool over = point_in_polygon(px, py, s.x + poly.start, s.y + poly.start, poly.n);
            for (int k = 0; !over && k < poly.n; k++) {
                  int a = poly.start + k, b = poly.start + (k + 1) % poly.n;
                  double cx, cy;
                  over = dist_to_segment_pt(s, px, py, s.x[a], s.y[a], s.x[b], s.y[b], cx, cy) < pt.radius;
            }
            if (!over) return NA_DBL;
            return interpolate_z(s, poly, px, py) - pz;
      }
      if (px < poly.xmin || px > poly.xmax || py < poly.ymin || py > poly.ymax) return NA_DBL;
      if (!point_in_polygon(px, py, s.x + poly.start, s.y + poly.start, poly.n)) return NA_DBL;
      return interpolate_z(s, poly, px, py) - pz;
}

double compare_poly_segment(const Scene& s, const Group& poly, const Group& seg,
                            double* xy_out, bool& has_xy) {
      if (seg.xmin >= poly.xmax || seg.xmax <= poly.xmin ||
          seg.ymin >= poly.ymax || seg.ymax <= poly.ymin) return NA_DBL;
      double mx, my;
      if (!segment_overlap_midpoint(s, poly, seg, mx, my)) return NA_DBL;
      xy_out[0] = mx; xy_out[1] = my; has_xy = true;
      int r = seg.start;
      double seg_z = interpolate_z_on_segment(s.x[r], s.y[r], s.d[r],
                                              s.x[r + 1], s.y[r + 1], s.d[r + 1], mx, my);
      return interpolate_z(s, poly, mx, my) - seg_z;
}

double compare_seg_seg(const Scene& s, const Group& i, const Group& j) {
      int a = i.start, b = j.start;
      double ix, iy;
      if (!segment_intersection(s.x[a], s.y[a], s.x[a + 1], s.y[a + 1],
                                s.x[b], s.y[b], s.x[b + 1], s.y[b + 1], ix, iy)) {
            if (!s.sized || i.halfwidth + j.halfwidth <= 0) return NA_DBL;
            // Lines that don't cross can still overlap where drawn: compare
            // them where they come closest, if that's within their widths
            double best = INFINITY, pi_x = 0, pi_y = 0, pj_x = 0, pj_y = 0;
            for (int e = 0; e < 4; e++) {
                  const Group& from = e < 2 ? i : j;
                  const Group& onto = e < 2 ? j : i;
                  int fr = from.start + (e % 2), on = onto.start;
                  double cx, cy;
                  double dd = dist_to_segment_pt(s, s.x[fr], s.y[fr], s.x[on], s.y[on],
                                                 s.x[on + 1], s.y[on + 1], cx, cy);
                  if (dd < best) {
                        best = dd;
                        if (e < 2) { pi_x = s.x[fr]; pi_y = s.y[fr]; pj_x = cx; pj_y = cy; }
                        else { pj_x = s.x[fr]; pj_y = s.y[fr]; pi_x = cx; pi_y = cy; }
                  }
            }
            if (best >= i.halfwidth + j.halfwidth) return NA_DBL;
            double zi = interpolate_z_on_segment(s.x[a], s.y[a], s.d[a], s.x[a + 1], s.y[a + 1], s.d[a + 1], pi_x, pi_y);
            double zj = interpolate_z_on_segment(s.x[b], s.y[b], s.d[b], s.x[b + 1], s.y[b + 1], s.d[b + 1], pj_x, pj_y);
            return zi - zj;
      }
      double zi = interpolate_z_on_segment(s.x[a], s.y[a], s.d[a], s.x[a + 1], s.y[a + 1], s.d[a + 1], ix, iy);
      double zj = interpolate_z_on_segment(s.x[b], s.y[b], s.d[b], s.x[b + 1], s.y[b + 1], s.d[b + 1], ix, iy);
      return zi - zj;
}

double compare_point_seg(const Scene& s, const Group& pt, const Group& seg) {
      double px = s.x[pt.start], py = s.y[pt.start], pz = s.d[pt.start];
      int r = seg.start;
      if (s.sized) {
            // The marker overlaps the drawn line
            double cx, cy;
            double dd = dist_to_segment_pt(s, px, py, s.x[r], s.y[r], s.x[r + 1], s.y[r + 1], cx, cy);
            if (dd >= pt.radius + seg.halfwidth) return NA_DBL;
            double seg_z = interpolate_z_on_segment(s.x[r], s.y[r], s.d[r], s.x[r + 1], s.y[r + 1], s.d[r + 1], cx, cy);
            return seg_z - pz;
      }
      if (px < seg.xmin || px > seg.xmax || py < seg.ymin || py > seg.ymax) return NA_DBL;
      double x1 = s.x[r], y1 = s.y[r], x2 = s.x[r + 1], y2 = s.y[r + 1];
      double dx = x2 - x1, dy = y2 - y1, len_sq = dx * dx + dy * dy;
      double cx = x1, cy = y1;
      if (len_sq >= 1e-20) {
            double t = ((px - x1) * dx + (py - y1) * dy) / len_sq;
            t = std::max(0.0, std::min(1.0, t));
            cx = x1 + t * dx; cy = y1 + t * dy;
      }
      double seg_z = interpolate_z_on_segment(x1, y1, s.d[r], x2, y2, s.d[r + 1], cx, cy);
      return seg_z - pz;
}

// Overlapping markers: the farther is drawn first
double compare_point_point(const Scene& s, const Group& i, const Group& j) {
      if (!s.sized || i.radius <= 0 || j.radius <= 0) return NA_DBL;
      double dd = dist_pt(s, s.x[i.start], s.y[i.start], s.x[j.start], s.y[j.start]);
      if (dd >= i.radius + j.radius) return NA_DBL;
      return s.d[i.start] - s.d[j.start];
}

// Dispatch on types, with tiebreakers for coplanar primitives: smaller
// primitives on top, then later layers on top
double compare_pair(const Scene& s, int ii, int jj, const double* given_xy,
                    bool& deferred, double* xy, bool& has_xy) {
      const Group& i = s.g[ii];
      const Group& j = s.g[jj];
      int ti = i.type, tj = j.type;
      double dlt = NA_DBL;
      if (ti == POLYGON && tj == POLYGON) {
            dlt = compare_poly_poly(s, i, j, given_xy, deferred, xy, has_xy);
      } else if (ti == POLYGON && is_pointlike(tj)) {
            dlt = compare_poly_point(s, i, j);
      } else if (tj == POLYGON && is_pointlike(ti)) {
            dlt = -compare_poly_point(s, j, i);
      } else if (ti == POLYGON && tj == SEGMENT) {
            dlt = compare_poly_segment(s, i, j, xy, has_xy);
      } else if (tj == POLYGON && ti == SEGMENT) {
            dlt = -compare_poly_segment(s, j, i, xy, has_xy);
      } else if (ti == SEGMENT && tj == SEGMENT) {
            dlt = compare_seg_seg(s, i, j);
      } else if (is_pointlike(ti) && tj == SEGMENT) {
            dlt = compare_point_seg(s, i, j);
      } else if (is_pointlike(tj) && ti == SEGMENT) {
            dlt = -compare_point_seg(s, j, i);
      } else if (ti == POINT && tj == POINT) {
            dlt = compare_point_point(s, i, j);
      }
      if (!std::isnan(dlt) && std::fabs(dlt) < COPLANAR_TOL) {
            double pi = prim_priority(ti), pj = prim_priority(tj);
            if (pi != pj) return pj - pi;
            if (i.layer != j.layer) return i.layer < j.layer ? COPLANAR_TOL : -COPLANAR_TOL;
      }
      return dlt;
}

// Exact 3D ordering ------------------------------------------------------------

// Which side of a polygon's plane band a set of points lies on: 1, -1, or 0
// if undecided (as side_of_plane() in R)
int side_of_plane(const std::vector<Vec3>& pts, const Plane& pl, double tol, double touch) {
      double lo = INFINITY, hi = -INFINITY;
      for (const Vec3& p : pts) {
            double d = dot3(p, pl.normal) - pl.offset;
            lo = std::min(lo, d); hi = std::max(hi, d);
      }
      double w = pl.warp;
      if (hi >= -lo) {
            if (lo >= w - tol - touch && hi > w + tol) return 1;
      } else {
            if (hi <= -w + tol + touch && lo < -w - tol) return -1;
      }
      return 0;
}

int eye_side_of_plane(const Plane& pl, const Eye& eye) {
      if (eye.persp) {
            double s = dot3_sum(pl.normal, eye.v) - pl.offset;
            if (std::fabs(s) <= pl.warp + 1e-9) return 0;
            return s > 0 ? 1 : -1;
      }
      double s = dot3_sum(pl.normal, eye.v);
      if (std::fabs(s) < 1e-9) return 0;
      return s > 0 ? 1 : -1;
}

int plane_side_order(const Group& gi, const Group& gj, const Eye& eye, double tol = 1e-6) {
      if (gi.plane.valid) {
            int s = side_of_plane(gj.pts, gi.plane, tol, gj.touch);
            int e = eye_side_of_plane(gi.plane, eye);
            if (s != 0 && e != 0) return s == e ? 1 : -1;
      }
      if (gj.plane.valid) {
            int s = side_of_plane(gi.pts, gj.plane, tol, gi.touch);
            int e = eye_side_of_plane(gj.plane, eye);
            if (s != 0 && e != 0) return s == e ? -1 : 1;
      }
      return 0;
}

bool in_plane(const Group& poly, const Group& other, double tol) {
      if (!poly.plane.valid || !poly.plane.planar) return false;
      if (other.type == POLYGON) return false;
      for (const Vec3& p : other.pts) {
            if (std::fabs(dot3(p, poly.plane.normal) - poly.plane.offset) > tol) return false;
      }
      return true;
}

int coplanar_order(const Group& gi, const Group& gj, double tol = 1e-6) {
      if (gi.type == POLYGON && in_plane(gi, gj, tol)) return 1;
      if (gj.type == POLYGON && in_plane(gj, gi, tol)) return -1;
      return 0;
}

// Line of sight through a screen point, in cube space (as screen_ray() in R)
void screen_ray(const Proj& p, double sx, double sy, Vec3& o, Vec3& d) {
      double xt = p.pb[0] + sx * (p.pb[1] - p.pb[0]);
      double yt = p.pb[2] + sy * (p.pb[3] - p.pb[2]);
      auto to_cube = [&](double a, double b, double c) {
            // f = v %*% t(rot): f_k = sum_m v_m rot[k, m]
            Vec3 f{a * p.rot[0] + b * p.rot[3] + c * p.rot[6],
                   a * p.rot[1] + b * p.rot[4] + c * p.rot[7],
                   a * p.rot[2] + b * p.rot[5] + c * p.rot[8]};
            f.z = -f.z;
            return f;
      };
      Vec3 p0 = to_cube(xt, yt, 0);
      if (p.persp) {
            o = to_cube(0, 0, -p.dist);
            d = sub3(p0, o);
      } else {
            o = p0;
            d = to_cube(0, 0, 1);
      }
}

double ray_depth(const Vec3& o, const Vec3& d, const Group& g) {
      if (g.type == POLYGON) {
            double den = dot3_sum(g.plane.normal, d);
            if (std::fabs(den) < 1e-12) return NA_DBL;
            return (g.plane.offset - dot3_sum(g.plane.normal, o)) / den;
      }
      Vec3 a = g.pts[0];
      Vec3 e = sub3(g.pts[1], a);
      Vec3 w = sub3(a, o);
      double dd = dot3_sum(d, d), de = dot3_sum(d, e), ee = dot3_sum(e, e);
      double den = dd * ee - de * de;
      if (std::fabs(den) < 1e-14) return NA_DBL;
      return (dot3_sum(d, w) * ee - de * dot3_sum(e, w)) / den;
}

// Whether two planar polygons lie in the same plane
bool coplanar_polygons(const Group& a, const Group& b, double tol = 1e-6) {
      if (a.type != POLYGON || b.type != POLYGON) return false;
      if (!a.plane.valid || !b.plane.valid || !a.plane.planar || !b.plane.planar) return false;
      double c = dot3(a.plane.normal, b.plane.normal);
      if (std::fabs(c) < 1 - 1e-9) return false;
      double off_b = c > 0 ? b.plane.offset : -b.plane.offset;
      return std::fabs(a.plane.offset - off_b) <= tol;
}

bool exact_ok(const Group& g) {
      return (g.type == POLYGON && g.plane.valid && g.plane.planar) ||
            (g.type == SEGMENT && g.pts.size() == 2);
}

// Order of two disjoint primitives from one exact line of sight through
// their screen overlap (as ray_sample_order() in R). Returns 2 when an
// intersection centroid is needed but must be computed in R.
int ray_sample_order(const Scene& s, int ii, int jj, const double* xy_known, bool has_xy,
                     const double* given_xy) {
      const Group& gi = s.g[ii];
      const Group& gj = s.g[jj];
      if (!s.proj.valid) return 0;
      if (!(exact_ok(gi) && exact_ok(gj))) return 0;
      if (gi.type != POLYGON && gj.type != POLYGON) return 0;
      double xy[2];
      if (has_xy) {
            xy[0] = xy_known[0]; xy[1] = xy_known[1];
      } else if (gi.type == POLYGON && gj.type == POLYGON) {
            if (given_xy) {
                  if (!std::isfinite(given_xy[0])) return 0;
                  xy[0] = given_xy[0]; xy[1] = given_xy[1];
            } else {
                  int found = intersection_centroid(s, gi, gj, xy[0], xy[1]);
                  if (found < 0) return 2;
                  if (found == 0) return 0;
            }
      } else if (gi.type == POLYGON) {
            if (!segment_overlap_midpoint(s, gi, gj, xy[0], xy[1])) return 0;
      } else {
            if (!segment_overlap_midpoint(s, gj, gi, xy[0], xy[1])) return 0;
      }
      Vec3 o, d;
      screen_ray(s.proj, xy[0], xy[1], o, d);
      double li = ray_depth(o, d, gi), lj = ray_depth(o, d, gj);
      if (!std::isfinite(li) || !std::isfinite(lj)) return 0;
      if (std::fabs(li - lj) <= 1e-9 * std::max(1.0, std::max(std::fabs(li), std::fabs(lj)))) return 0;
      return li > lj ? 1 : -1;
}

// Full comparison of one pair. Returns false if deferred to R.
bool resolve_pair(const Scene& s, int i, int j, const double* given_xy,
                  double& strength, bool& locked) {
      bool deferred = false, has_xy = false;
      double xy[2];
      double dlt = compare_pair(s, i, j, given_xy, deferred, xy, has_xy);
      if (deferred) return false;
      locked = false;
      strength = dlt;
      if (std::isnan(dlt)) return true;
      const Group& gi = s.g[i];
      const Group& gj = s.g[j];
      if (s.eye.valid && gi.has3d && gj.has3d) {
            int exact = plane_side_order(gi, gj, s.eye);
            if (exact == 0) exact = coplanar_order(gi, gj);
            if (exact == 0 && gi.layer != gj.layer && coplanar_polygons(gi, gj)) {
                  // Coplanar polygons from different layers: the later on top
                  strength = gi.layer < gj.layer ? COPLANAR_TOL : -COPLANAR_TOL;
                  return true;
            }
            if (exact == 0) {
                  exact = ray_sample_order(s, i, j, xy, has_xy, given_xy);
                  if (exact == 2) return false;
            }
            if (exact != 0) {
                  strength = exact * std::max(std::fabs(dlt), COPLANAR_TOL);
                  locked = true;
            }
      }
      return true;
}

Scene build_scene(doubles x, doubles y, doubles d, doubles x3, doubles y3, doubles z3,
                  integers offsets, integers type, integers cut_parent,
                  doubles eye, doubles proj, list marks = list()) {
      Scene s;
      s.x = REAL(x); s.y = REAL(y); s.d = REAL(d);
      int k = type.size();
      bool have3d = x3.size() == x.size();
      s.g.resize(k);
      for (int gi = 0; gi < k; gi++) {
            Group& g = s.g[gi];
            g.start = offsets[gi];
            g.n = offsets[gi + 1] - offsets[gi];
            g.type = type[gi];
            g.cut_parent = cut_parent[gi];
            g.xmin = g.ymin = g.dmin = INFINITY;
            g.xmax = g.ymax = g.dmax = -INFINITY;
            double sum = 0;
            for (int r = g.start; r < g.start + g.n; r++) {
                  g.xmin = std::min(g.xmin, s.x[r]); g.xmax = std::max(g.xmax, s.x[r]);
                  g.ymin = std::min(g.ymin, s.y[r]); g.ymax = std::max(g.ymax, s.y[r]);
                  g.dmin = std::min(g.dmin, s.d[r]); g.dmax = std::max(g.dmax, s.d[r]);
                  sum += s.d[r];
            }
            g.dmean = sum / g.n;
            if (g.type == POLYGON) {
                  if (g.n >= 3) g.has_beta = fit_beta(s.x + g.start, s.y + g.start, s.d + g.start, g.n, g.beta);
                  g.convex = ring_convex(s.x + g.start, s.y + g.start, g.n);
            }
            if (have3d) {
                  bool ok = true;
                  g.pts.resize(g.n);
                  for (int q = 0; q < g.n; q++) {
                        int r = g.start + q;
                        g.pts[q] = {x3[r], y3[r], z3[r]};
                        if (ISNAN(x3[r]) || ISNAN(y3[r]) || ISNAN(z3[r])) ok = false;
                  }
                  g.has3d = ok;
                  if (ok && g.type == POLYGON && g.n >= 3) g.plane = polygon_plane(g.pts);
                  g.touch = g.type == SEGMENT ? SPLIT_TOL : 0;
            }
      }
      if (eye.size() == 4) {
            s.eye.valid = true;
            s.eye.persp = eye[0] != 0;
            s.eye.v = {eye[1], eye[2], eye[3]};
      }
      // Optional marker extents, layers, and panel size
      if (marks.size() > 0) {
            doubles panel(marks["panel"]);
            doubles radius(marks["radius"]);
            doubles halfwidth(marks["halfwidth"]);
            integers layer(marks["layer"]);
            if (panel.size() == 2 && panel[0] > 0 && panel[1] > 0) {
                  s.sized = true;
                  s.pw = panel[0];
                  s.ph = panel[1];
            }
            for (int gi = 0; gi < k; gi++) {
                  if (radius.size() == k && !ISNAN(radius[gi])) s.g[gi].radius = radius[gi];
                  if (halfwidth.size() == k && !ISNAN(halfwidth[gi])) s.g[gi].halfwidth = halfwidth[gi];
                  if (layer.size() == k) s.g[gi].layer = layer[gi];
            }
      }
      if (proj.size() == 15) {
            s.proj.valid = true;
            s.proj.persp = proj[0] != 0;
            s.proj.dist = proj[1];
            for (int q = 0; q < 9; q++) s.proj.rot[q] = proj[2 + q];
            for (int q = 0; q < 4; q++) s.proj.pb[q] = proj[11 + q];
      }
      return s;
}

bool eligible(const Scene& s, int i, int j) {
      const Group& a = s.g[i];
      const Group& b = s.g[j];
      if (is_pointlike(a.type) && is_pointlike(b.type)) {
            if (!(s.sized && a.type == POINT && b.type == POINT && a.radius > 0 && b.radius > 0)) return false;
      }
      if (a.cut_parent >= 0 && a.cut_parent == b.cut_parent) return false;
      return true;
}

} // namespace


// Candidate pairs: groups whose screen bounding boxes overlap (inclusive),
// found by sweeping over x, excluding pairs that can't constrain each other.
// Returned 0-based with i < j.
static std::vector<std::pair<int, int>> candidate_pairs(const Scene& s) {
      int k = s.g.size();
      // Screen boxes, widened by marker and line extents when known
      std::vector<double> x0(k), x1(k), y0(k), y1(k);
      for (int q = 0; q < k; q++) {
            const Group& g = s.g[q];
            double ex = 0, ey = 0;
            if (s.sized) {
                  double e = std::max(g.radius, g.halfwidth);
                  ex = e / s.pw; ey = e / s.ph;
            }
            x0[q] = g.xmin - ex; x1[q] = g.xmax + ex; y0[q] = g.ymin - ey; y1[q] = g.ymax + ey;
      }
      std::vector<int> ord(k);
      std::iota(ord.begin(), ord.end(), 0);
      std::sort(ord.begin(), ord.end(), [&](int a, int b) {
            return x0[a] < x0[b] || (x0[a] == x0[b] && a < b);
      });
      std::vector<std::pair<int, int>> out;
      for (int p = 0; p < k; p++) {
            int a = ord[p];
            if (!std::isfinite(x0[a])) continue;
            for (int q = p + 1; q < k; q++) {
                  int b = ord[q];
                  if (x0[b] > x1[a]) break;
                  if (y0[b] > y1[a] || y1[b] < y0[a]) continue;
                  int i = std::min(ord[p], ord[q]), j = std::max(ord[p], ord[q]);
                  if (!eligible(s, i, j)) continue;
                  out.emplace_back(i, j);
            }
      }
      return out;
}

[[cpp11::register]]
list pw_constraints_cpp(doubles x, doubles y, doubles d, doubles x3, doubles y3, doubles z3,
                        integers offsets, integers type, integers cut_parent,
                        doubles eye, doubles proj, list marks) {
      Scene s = build_scene(x, y, d, x3, y3, z3, offsets, type, cut_parent, eye, proj, marks);
      std::vector<std::pair<int, int>> cand = candidate_pairs(s);

      writable::integers from, to, def_i, def_j;
      writable::doubles strength;
      writable::logicals locked;
      for (const auto& pr : cand) {
            double st;
            bool lk;
            if (!resolve_pair(s, pr.first, pr.second, nullptr, st, lk)) {
                  def_i.push_back(pr.first);
                  def_j.push_back(pr.second);
                  continue;
            }
            if (std::isnan(st) || st == 0) continue;
            // Orient each constraint behind -> front
            if (st > 0) { from.push_back(pr.first); to.push_back(pr.second); }
            else { from.push_back(pr.second); to.push_back(pr.first); }
            strength.push_back(std::fabs(st));
            locked.push_back(lk ? TRUE : FALSE);
      }
      writable::doubles depth(s.g.size());
      for (size_t q = 0; q < s.g.size(); q++) depth[q] = s.g[q].dmean;

      return writable::list({
            "from"_nm = from, "to"_nm = to, "strength"_nm = strength, "locked"_nm = locked,
            "deferred_i"_nm = def_i, "deferred_j"_nm = def_j,
            "n_candidates"_nm = (int) cand.size(), "depth"_nm = depth});
}

// Finishes deferred pairs given their screen intersection centroids (NaN
// where the polygons don't intersect)
[[cpp11::register]]
list pw_finish_pairs_cpp(doubles x, doubles y, doubles d, doubles x3, doubles y3, doubles z3,
                         integers offsets, integers type, integers cut_parent,
                         doubles eye, doubles proj, list marks,
                         integers pi, integers pj, doubles cx, doubles cy) {
      Scene s = build_scene(x, y, d, x3, y3, z3, offsets, type, cut_parent, eye, proj, marks);
      writable::integers from, to;
      writable::doubles strength;
      writable::logicals locked;
      for (R_xlen_t q = 0; q < pi.size(); q++) {
            double xy[2] = {cx[q], cy[q]};
            double st;
            bool lk;
            if (!resolve_pair(s, pi[q], pj[q], xy, st, lk)) continue;
            if (std::isnan(st) || st == 0) continue;
            if (st > 0) { from.push_back(pi[q]); to.push_back(pj[q]); }
            else { from.push_back(pj[q]); to.push_back(pi[q]); }
            strength.push_back(std::fabs(st));
            locked.push_back(lk ? TRUE : FALSE);
      }
      return writable::list({"from"_nm = from, "to"_nm = to,
                             "strength"_nm = strength, "locked"_nm = locked});
}

// The steps of comparing groups 0 and 1 of a scene, for the R wrappers used
// in tests: the screen-space delta, the exact plane-side, coplanar, and
// ray-sample orders (NA when undecided), and the resolved constraint
[[cpp11::register]]
list pair_order_cpp(doubles x, doubles y, doubles d, doubles x3, doubles y3, doubles z3,
                    integers offsets, integers type, doubles eye, doubles proj) {
      writable::integers cut(type.size());
      for (R_xlen_t q = 0; q < type.size(); q++) cut[q] = -1;
      Scene s = build_scene(x, y, d, x3, y3, z3, offsets, type, cut, eye, proj);
      bool deferred = false, has_xy = false;
      double xy[2];
      double screen = compare_pair(s, 0, 1, nullptr, deferred, xy, has_xy);
      int plane = 0, cop = 0, ray = 0;
      if (s.g[0].has3d && s.g[1].has3d) {
            if (s.eye.valid) plane = plane_side_order(s.g[0], s.g[1], s.eye);
            cop = coplanar_order(s.g[0], s.g[1]);
            ray = ray_sample_order(s, 0, 1, xy, has_xy, nullptr);
      }
      double st = NA_DBL;
      bool lk = false;
      bool done = resolve_pair(s, 0, 1, nullptr, st, lk);
      auto as_int = [](int v) { return v == 0 || v == 2 ? NA_INTEGER : v; };
      return writable::list({
            "screen"_nm = deferred ? NA_DBL : screen,
            "plane_side"_nm = as_int(plane), "coplanar"_nm = as_int(cop), "ray"_nm = as_int(ray),
            "resolved"_nm = done ? st : NA_DBL, "locked"_nm = lk, "deferred"_nm = deferred || !done});
}

// Midpoint of the portion of a screen segment inside a screen polygon
[[cpp11::register]]
doubles segment_overlap_midpoint_cpp(doubles px, doubles py, doubles sx, doubles sy) {
      std::vector<double> vx, vy;
      int pieces = segment_inside_pieces(sx[0], sy[0], sx[1], sy[1], REAL(px), REAL(py), px.size(), vx, vy);
      if (pieces == 0) return writable::doubles();
      writable::doubles out(2);
      double mx = 0, my = 0;
      longest_piece_midpoint(vx, vy, mx, my);
      out[0] = mx;
      out[1] = my;
      return out;
}

// Line of sight through a screen point, in cube space
[[cpp11::register]]
list screen_ray_cpp(double sx, double sy, doubles proj) {
      Proj p;
      p.valid = true;
      p.persp = proj[0] != 0;
      p.dist = proj[1];
      for (int q = 0; q < 9; q++) p.rot[q] = proj[2 + q];
      for (int q = 0; q < 4; q++) p.pb[q] = proj[11 + q];
      Vec3 o, d;
      screen_ray(p, sx, sy, o, d);
      return writable::list({"o"_nm = writable::doubles({o.x, o.y, o.z}),
                             "d"_nm = writable::doubles({d.x, d.y, d.z})});
}

// A representative interior point of a screen ring (see interior_point())
[[cpp11::register]]
doubles interior_point_cpp(doubles x, doubles y) {
      std::vector<double> vx(x.begin(), x.end()), vy(y.begin(), y.end());
      double cx = 0, cy = 0, area = 0, px = 0, py = 0;
      if (!ring_centroid(vx, vy, cx, cy, area)) return writable::doubles({vx[0], vy[0]});
      interior_point(vx, vy, cx, cy, px, py);
      return writable::doubles({px, py});
}
