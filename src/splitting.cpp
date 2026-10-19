// Splitting of interpenetrating primitives before pairwise sorting
//
// Painter's algorithm can't order two polygons that pass through each other,
// or a segment passing through a polygon. These passes find such pairs in
// standardized cube space, where planes are exactly planar under both
// orthographic and perspective projection, and cut one member of each pair
// along the other's plane. Candidate pairs are those whose 3D bounding boxes
// overlap, found by sweeping over x.
//
// The logic, tolerances, and ordering follow the R implementation they
// replaced; projecting new vertices and assembling rows stays in R.

#include <cpp11.hpp>
#include <vector>
#include <cmath>
#include <algorithm>
#include <numeric>
#include "geometry.h"

using namespace cpp11;
using namespace ggcube;

namespace {

inline Vec3 cross3(const Vec3& a, const Vec3& b) {
      return {a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x};
}

struct Poly {
      int start, n;
      std::vector<Vec3> pts;
      Plane plane;
      bool cuttable;
      Vec3 lo, hi;
};

std::vector<Poly> read_polys(const doubles& x3, const doubles& y3, const doubles& z3,
                             const integers& offsets, const logicals& cuttable) {
      int k = offsets.size() - 1;
      std::vector<Poly> polys(k);
      for (int g = 0; g < k; g++) {
            Poly& p = polys[g];
            p.start = offsets[g];
            p.n = offsets[g + 1] - offsets[g];
            p.pts.resize(p.n);
            p.lo = {INFINITY, INFINITY, INFINITY};
            p.hi = {-INFINITY, -INFINITY, -INFINITY};
            for (int q = 0; q < p.n; q++) {
                  int r = p.start + q;
                  Vec3 v{x3[r], y3[r], z3[r]};
                  p.pts[q] = v;
                  p.lo = {std::min(p.lo.x, v.x), std::min(p.lo.y, v.y), std::min(p.lo.z, v.z)};
                  p.hi = {std::max(p.hi.x, v.x), std::max(p.hi.y, v.y), std::max(p.hi.z, v.z)};
            }
            p.plane = polygon_plane(p.pts);
            p.cuttable = cuttable[g] == TRUE;
      }
      return polys;
}

// Range of a polygon's plane crossings projected onto a direction (as
// crossing_interval() in R)
void crossing_interval(const std::vector<Vec3>& pts, std::vector<double> d, double eps,
                       const Vec3& u, double& lo, double& hi) {
      size_t n = d.size();
      for (double& v : d) if (std::fabs(v) <= eps) v = 0;
      lo = INFINITY; hi = -INFINITY;
      for (size_t i = 0; i < n; i++) {
            size_t k = (i + 1) % n;
            if ((d[i] > 0 && d[k] < 0) || (d[i] < 0 && d[k] > 0)) {
                  double t = d[i] / (d[i] - d[k]);
                  Vec3 p{pts[i].x + t * (pts[k].x - pts[i].x),
                         pts[i].y + t * (pts[k].y - pts[i].y),
                         pts[i].z + t * (pts[k].z - pts[i].z)};
                  double s = dot3(p, u);
                  lo = std::min(lo, s); hi = std::max(hi, s);
            }
      }
      for (size_t i = 0; i < n; i++) {
            if (d[i] == 0) {
                  double s = dot3(pts[i], u);
                  lo = std::min(lo, s); hi = std::max(hi, s);
            }
      }
}

// Whether two polygons pass through each other (as polygons_interpenetrate()
// in R)
bool polygons_interpenetrate(const Poly& a, const Poly& b, double tol) {
      double eps_a = tol + a.plane.warp, eps_b = tol + b.plane.warp;
      std::vector<double> da(a.n), db(b.n);
      double mx = -INFINITY, mn = INFINITY;
      for (int q = 0; q < a.n; q++) {
            da[q] = dot3(a.pts[q], b.plane.normal) - b.plane.offset;
            mx = std::max(mx, da[q]); mn = std::min(mn, da[q]);
      }
      if (!(mx > eps_b && mn < -eps_b)) return false;
      mx = -INFINITY; mn = INFINITY;
      for (int q = 0; q < b.n; q++) {
            db[q] = dot3(b.pts[q], a.plane.normal) - a.plane.offset;
            mx = std::max(mx, db[q]); mn = std::min(mn, db[q]);
      }
      if (!(mx > eps_a && mn < -eps_a)) return false;
      Vec3 u = cross3(a.plane.normal, b.plane.normal);
      double ul = std::sqrt(dot3_sum(u, u));
      if (ul < 1e-9) return false;
      u = {u.x / ul, u.y / ul, u.z / ul};
      double lo_a, hi_a, lo_b, hi_b;
      crossing_interval(a.pts, da, eps_b, u, lo_a, hi_a);
      crossing_interval(b.pts, db, eps_a, u, lo_b, hi_b);
      return std::min(hi_a, hi_b) - std::max(lo_a, lo_b) > tol;
}

struct Ring {
      std::vector<Vec3> p;
      std::vector<int> src;
      std::vector<char> is_new, flag;
};

// Keeps the non-negative side of a ring (as clip_ring_side() in R)
Ring clip_ring_side(const Ring& ring, const std::vector<double>& dd) {
      Ring out;
      size_t n = dd.size();
      for (size_t i = 0; i < n; i++) {
            size_t j = i == n - 1 ? 0 : i + 1;
            if (dd[i] >= 0) {
                  out.p.push_back(ring.p[i]);
                  out.src.push_back(ring.src[i]);
                  out.is_new.push_back(ring.is_new[i]);
                  out.flag.push_back((dd[i] == 0 && dd[j] < 0) ? 1 : ring.flag[i]);
            }
            if ((dd[i] > 0 && dd[j] < 0) || (dd[i] < 0 && dd[j] > 0)) {
                  double t = dd[i] / (dd[i] - dd[j]);
                  const Vec3& a = ring.p[i];
                  const Vec3& b = ring.p[j];
                  out.p.push_back({a.x + t * (b.x - a.x), a.y + t * (b.y - a.y), a.z + t * (b.z - a.z)});
                  out.src.push_back(ring.src[i]);
                  out.is_new.push_back(1);
                  out.flag.push_back(dd[i] > 0 ? 1 : ring.flag[i]);
            }
      }
      return out;
}

// Splits a ring by a plane into two rings, or leaves it whole (as
// split_ring_by_plane() in R)
std::vector<Ring> split_ring_by_plane(const Ring& ring, const Plane& pl, double tol) {
      size_t n = ring.p.size();
      std::vector<double> d(n);
      bool pos = false, neg = false;
      for (size_t i = 0; i < n; i++) {
            d[i] = dot3(ring.p[i], pl.normal) - pl.offset;
            if (std::fabs(d[i]) <= tol) d[i] = 0;
            pos = pos || d[i] > 0;
            neg = neg || d[i] < 0;
      }
      if (!(pos && neg)) return {ring};
      std::vector<double> nd(n);
      for (size_t i = 0; i < n; i++) nd[i] = -d[i];
      std::vector<Ring> sides;
      Ring a = clip_ring_side(ring, d);
      if (a.p.size() >= 3) sides.push_back(a);
      Ring b = clip_ring_side(ring, nd);
      if (b.p.size() >= 3) sides.push_back(b);
      if (sides.size() < 2) return {ring};
      return sides;
}

// Pairs (i < j) whose boxes overlap strictly on every axis, after expanding
// each box by `pad`, sorted by j then i
std::vector<std::pair<int, int>> box_pairs(const std::vector<Vec3>& lo, const std::vector<Vec3>& hi,
                                           const std::vector<int>& ids) {
      std::vector<int> ord(ids);
      std::sort(ord.begin(), ord.end(), [&](int a, int b) {
            return lo[a].x < lo[b].x || (lo[a].x == lo[b].x && a < b);
      });
      std::vector<std::pair<int, int>> out;
      for (size_t p = 0; p < ord.size(); p++) {
            int a = ord[p];
            for (size_t q = p + 1; q < ord.size(); q++) {
                  int b = ord[q];
                  if (lo[b].x >= hi[a].x) break;
                  if (!(lo[a].y < hi[b].y && hi[a].y > lo[b].y)) continue;
                  if (!(lo[a].z < hi[b].z && hi[a].z > lo[b].z)) continue;
                  if (!(lo[a].x < hi[b].x && hi[a].x > lo[b].x)) continue;
                  out.emplace_back(std::min(a, b), std::max(a, b));
            }
      }
      std::sort(out.begin(), out.end(), [](const std::pair<int, int>& u, const std::pair<int, int>& v) {
            return u.second < v.second || (u.second == v.second && u.first < v.first);
      });
      return out;
}

} // namespace


// Cuts polygons that interpenetrate other polygons. Takes the cube-space
// vertices of every candidate polygon (in CSR form) and whether each may be
// cut (polygons with holes may not). Returns the pieces of every polygon cut
// into two or more: for each piece vertex, the polygon (0-based), piece
// number (1-based), source vertex (0-based row in the input arrays),
// coordinates, and whether it is new and starts a cut edge.
[[cpp11::register]]
list split_polygons_cpp(doubles x3, doubles y3, doubles z3, integers offsets, logicals cuttable,
                        double tol) {
      std::vector<Poly> polys = read_polys(x3, y3, z3, offsets, cuttable);
      int k = polys.size();
      std::vector<int> ids;
      std::vector<Vec3> lo(k), hi(k);
      for (int g = 0; g < k; g++) {
            lo[g] = polys[g].lo; hi[g] = polys[g].hi;
            if (polys[g].n >= 3 && polys[g].plane.valid) ids.push_back(g);
      }

      std::vector<std::vector<int>> cuts(k);
      for (const auto& pr : box_pairs(lo, hi, ids)) {
            const Poly& a = polys[pr.first];
            const Poly& b = polys[pr.second];
            bool ok_a = a.cuttable && b.plane.planar;
            bool ok_b = b.cuttable && a.plane.planar;
            if (!ok_a && !ok_b) continue;
            if (!polygons_interpenetrate(a, b, tol)) continue;
            bool cut_a = (ok_a && ok_b) ? a.plane.area <= b.plane.area : ok_a;
            if (cut_a) cuts[pr.first].push_back(pr.second);
            else cuts[pr.second].push_back(pr.first);
      }

      writable::integers o_poly, o_piece, o_src;
      writable::doubles o_x, o_y, o_z;
      writable::logicals o_new, o_flag;
      for (int w = 0; w < k; w++) {
            if (cuts[w].empty()) continue;
            const Poly& p = polys[w];
            Ring whole;
            whole.p = p.pts;
            whole.src.resize(p.n);
            std::iota(whole.src.begin(), whole.src.end(), p.start);
            whole.is_new.assign(p.n, 0);
            whole.flag.assign(p.n, 0);
            std::vector<Ring> pieces{whole};
            for (int c : cuts[w]) {
                  std::vector<Ring> next;
                  for (const Ring& r : pieces) {
                        std::vector<Ring> sp = split_ring_by_plane(r, polys[c].plane, tol);
                        next.insert(next.end(), sp.begin(), sp.end());
                  }
                  pieces.swap(next);
            }
            if (pieces.size() < 2) continue;
            for (size_t pc = 0; pc < pieces.size(); pc++) {
                  const Ring& r = pieces[pc];
                  for (size_t q = 0; q < r.p.size(); q++) {
                        o_poly.push_back(w);
                        o_piece.push_back((int) pc + 1);
                        o_src.push_back(r.src[q]);
                        o_x.push_back(r.p[q].x); o_y.push_back(r.p[q].y); o_z.push_back(r.p[q].z);
                        o_new.push_back(r.is_new[q] ? TRUE : FALSE);
                        o_flag.push_back(r.flag[q] ? TRUE : FALSE);
                  }
            }
      }
      return writable::list({"poly"_nm = o_poly, "piece"_nm = o_piece, "src"_nm = o_src,
                             "x3"_nm = o_x, "y3"_nm = o_y, "z3"_nm = o_z,
                             "is_new"_nm = o_new, "cut_edge"_nm = o_flag});
}


// Candidate crossings of segments through polygon planes. Segments are given
// by their start (a) and end (b) points; polygons by cube-space vertices in
// CSR form. A crossing requires the segment's ends to lie on opposite sides
// of the polygon's plane, each by more than `split_tol` plus the polygon's
// warp, within boxes overlapping after padding by `split_tol` (as in
// split_interpenetrating_segments() in R). Whether the crossing falls inside
// the polygon is tested afterwards, on screen. Returned in order of segment,
// then polygon (both 0-based), with the crossing parameter t and point.
[[cpp11::register]]
list segment_crossings_cpp(doubles ax, doubles ay, doubles az, doubles bx, doubles by, doubles bz,
                           doubles x3, doubles y3, doubles z3, integers offsets, double split_tol) {
      writable::logicals cut(offsets.size() - 1);
      for (R_xlen_t q = 0; q < cut.size(); q++) cut[q] = FALSE;
      std::vector<Poly> polys = read_polys(x3, y3, z3, offsets, cut);
      std::vector<int> ord;
      for (int g = 0; g < (int) polys.size(); g++) if (polys[g].plane.valid) ord.push_back(g);
      std::sort(ord.begin(), ord.end(), [&](int a, int b) { return polys[a].lo.x < polys[b].lo.x; });
      std::vector<double> lo_x(ord.size());
      for (size_t q = 0; q < ord.size(); q++) lo_x[q] = polys[ord[q]].lo.x;

      writable::integers o_seg, o_poly;
      writable::doubles o_t, o_x, o_y, o_z;
      for (R_xlen_t s = 0; s < ax.size(); s++) {
            Vec3 a{ax[s], ay[s], az[s]}, b{bx[s], by[s], bz[s]};
            Vec3 slo{std::min(a.x, b.x), std::min(a.y, b.y), std::min(a.z, b.z)};
            Vec3 shi{std::max(a.x, b.x), std::max(a.y, b.y), std::max(a.z, b.z)};
            size_t end = std::upper_bound(lo_x.begin(), lo_x.end(), shi.x + split_tol) - lo_x.begin();
            std::vector<int> near;
            for (size_t q = 0; q < end; q++) {
                  const Poly& p = polys[ord[q]];
                  if (p.lo.x <= shi.x + split_tol && p.lo.y <= shi.y + split_tol && p.lo.z <= shi.z + split_tol &&
                      p.hi.x >= slo.x - split_tol && p.hi.y >= slo.y - split_tol && p.hi.z >= slo.z - split_tol) {
                        near.push_back(ord[q]);
                  }
            }
            std::sort(near.begin(), near.end());
            for (int g : near) {
                  const Plane& pl = polys[g].plane;
                  double eps = split_tol + pl.warp;
                  double da = dot3(a, pl.normal) - pl.offset;
                  double db = dot3(b, pl.normal) - pl.offset;
                  if (da * db >= 0 || std::min(std::fabs(da), std::fabs(db)) < eps) continue;
                  double t = da / (da - db);
                  o_seg.push_back((int) s);
                  o_poly.push_back(g);
                  o_t.push_back(t);
                  o_x.push_back(a.x + t * (b.x - a.x));
                  o_y.push_back(a.y + t * (b.y - a.y));
                  o_z.push_back(a.z + t * (b.z - a.z));
            }
      }
      return writable::list({"seg"_nm = o_seg, "poly"_nm = o_poly, "t"_nm = o_t,
                             "x"_nm = o_x, "y"_nm = o_y, "z"_nm = o_z});
}


// Whether each screen point lies inside a given polygon (even-odd); polygons
// in CSR form, `poly` 0-based
[[cpp11::register]]
logicals points_in_polygons_cpp(doubles px, doubles py, integers poly,
                                doubles sx, doubles sy, integers offsets) {
      R_xlen_t n = px.size();
      writable::logicals out(n);
      for (R_xlen_t q = 0; q < n; q++) {
            int g = poly[q];
            int st = offsets[g], len = offsets[g + 1] - offsets[g];
            out[q] = point_in_polygon(px[q], py[q], REAL(sx) + st, REAL(sy) + st, len) ? TRUE : FALSE;
      }
      return out;
}
