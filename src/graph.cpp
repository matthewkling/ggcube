// Constraint graph operations for depth sorting
//
// Nodes are primitives (0-based); a directed edge from -> to means `from` is
// behind `to` and must be drawn first. Edges carry a strength (the magnitude
// of the depth difference behind the constraint) and a locked flag (set for
// constraints decided exactly from 3D geometry).

#include <cpp11.hpp>
#include <vector>
#include <queue>
#include <algorithm>
#include <functional>
#include <tuple>

using namespace cpp11;

namespace {

struct Graph {
      int n;
      std::vector<std::vector<int>> out;   // edge ids leaving each node
      std::vector<int> from, to;
      std::vector<bool> alive;
};

Graph make_graph(int n, const integers& from, const integers& to) {
      Graph g;
      g.n = n;
      g.out.assign(n, {});
      R_xlen_t m = from.size();
      g.from.resize(m); g.to.resize(m); g.alive.assign(m, true);
      for (R_xlen_t e = 0; e < m; e++) {
            g.from[e] = from[e];
            g.to[e] = to[e];
            g.out[from[e]].push_back(e);
      }
      return g;
}

// Tarjan's strongly connected components (iterative), restricted to nodes
// with `active` set and to live edges. Returns a component id per node (-1
// for inactive nodes).
std::vector<int> scc(const Graph& g, const std::vector<char>& active) {
      int n = g.n;
      std::vector<int> index(n, -1), low(n, 0), comp(n, -1);
      std::vector<char> on_stack(n, 0);
      std::vector<int> stack;
      std::vector<std::pair<int, size_t>> call;   // (node, next edge position)
      int counter = 0, ncomp = 0;
      for (int s = 0; s < n; s++) {
            if (!active[s] || index[s] >= 0) continue;
            call.push_back({s, 0});
            index[s] = low[s] = counter++;
            stack.push_back(s); on_stack[s] = 1;
            while (!call.empty()) {
                  int u = call.back().first;
                  size_t& pos = call.back().second;
                  if (pos < g.out[u].size()) {
                        int e = g.out[u][pos++];
                        if (!g.alive[e]) continue;
                        int v = g.to[e];
                        if (!active[v]) continue;
                        if (index[v] < 0) {
                              index[v] = low[v] = counter++;
                              stack.push_back(v); on_stack[v] = 1;
                              call.push_back({v, 0});
                        } else if (on_stack[v]) {
                              low[u] = std::min(low[u], index[v]);
                        }
                  } else {
                        call.pop_back();
                        if (!call.empty()) {
                              int parent = call.back().first;
                              low[parent] = std::min(low[parent], low[u]);
                        }
                        if (low[u] == index[u]) {
                              int w;
                              do {
                                    w = stack.back(); stack.pop_back();
                                    on_stack[w] = 0;
                                    comp[w] = ncomp;
                              } while (w != u);
                              ncomp++;
                        }
                  }
            }
      }
      return comp;
}

} // namespace


[[cpp11::register]]
integers scc_cpp(int n, integers from, integers to) {
      Graph g = make_graph(n, from, to);
      std::vector<char> active(n, 1);
      std::vector<int> comp = scc(g, active);
      writable::integers out(n);
      for (int i = 0; i < n; i++) out[i] = comp[i] + 1;
      return out;
}

// Removes constraints until the graph is acyclic. Only edges joining two
// members of the same strongly connected component lie on a cycle and are
// candidates. Within a component, unlocked edges go before locked ones, then
// weaker before stronger; ties go to the edge with the smallest `to`, then
// smallest `from` (the order R's column-major `which()` gave). Components
// are independent, so each is processed on its own, recomputing components
// within it after each removal. Returns a logical vector of edges kept.
[[cpp11::register]]
logicals break_cycles_cpp(int n, integers from, integers to, doubles strength, logicals locked) {
      Graph g = make_graph(n, from, to);
      R_xlen_t m = from.size();

      std::vector<char> all(n, 1);
      std::vector<int> comp = scc(g, all);

      // Work list of node sets forming nontrivial components
      std::vector<std::vector<int>> work;
      {
            std::vector<std::vector<int>> members(n);
            for (int i = 0; i < n; i++) members[comp[i]].push_back(i);
            for (auto& mb : members) if (mb.size() > 1) work.push_back(mb);
      }

      std::vector<char> active(n, 0);
      while (!work.empty()) {
            std::vector<int> nodes = std::move(work.back());
            work.pop_back();
            for (int v : nodes) active[v] = 1;
            std::vector<int> c = scc(g, active);

            // Split into the components found among these nodes
            std::vector<std::vector<int>> parts;
            {
                  std::vector<int> id_map;
                  std::vector<int> seen;
                  std::vector<std::vector<int>> by;
                  for (int v : nodes) {
                        int cid = c[v];
                        auto it = std::find(seen.begin(), seen.end(), cid);
                        if (it == seen.end()) { seen.push_back(cid); by.push_back({v}); }
                        else by[it - seen.begin()].push_back(v);
                  }
                  for (auto& b : by) if (b.size() > 1) parts.push_back(b);
            }
            for (int v : nodes) active[v] = 0;
            if (parts.empty()) continue;
            if (parts.size() > 1) {
                  for (auto& p : parts) work.push_back(p);
                  continue;
            }

            // One cyclic component: remove its weakest on-cycle edge
            const std::vector<int>& mem = parts[0];
            int best = -1;
            for (int v : mem) {
                  for (int e : g.out[v]) {
                        if (!g.alive[e]) continue;
                        int w = g.to[e];
                        if (c[w] != c[v]) continue;
                        if (best < 0) { best = e; continue; }
                        auto key = [&](int x) {
                              return std::make_tuple(locked[x] == TRUE ? 1 : 0, (double) strength[x],
                                                     g.to[x], g.from[x]);
                        };
                        if (key(e) < key(best)) best = e;
                  }
            }
            if (best < 0) continue;
            g.alive[best] = false;
            work.push_back(mem);
      }

      writable::logicals keep(m);
      for (R_xlen_t e = 0; e < m; e++) keep[e] = g.alive[e] ? TRUE : FALSE;
      return keep;
}

// Topological order (0-based), drawing the farthest available node first.
// Ties in depth go to the node that became available first, and nodes
// becoming available at the same step enter in increasing index order (as
// the stable re-sorting of the R implementation). Nodes left over by a cycle
// are appended in index order, with `complete` set to FALSE.
[[cpp11::register]]
list topological_sort_cpp(int n, integers from, integers to, doubles depth) {
      std::vector<std::vector<int>> out(n);
      std::vector<int> indeg(n, 0);
      for (R_xlen_t e = 0; e < from.size(); e++) {
            out[from[e]].push_back(to[e]);
            indeg[to[e]]++;
      }
      for (auto& o : out) std::sort(o.begin(), o.end());

      // Min-heap on (-depth, insertion sequence)
      typedef std::tuple<double, long, int> Item;
      std::priority_queue<Item, std::vector<Item>, std::greater<Item>> heap;
      long seq = 0;
      for (int i = 0; i < n; i++) if (indeg[i] == 0) heap.emplace(-depth[i], seq++, i);

      writable::integers order;
      std::vector<char> done(n, 0);
      while (!heap.empty()) {
            int u = std::get<2>(heap.top());
            heap.pop();
            order.push_back(u);
            done[u] = 1;
            for (int v : out[u]) {
                  if (--indeg[v] == 0) heap.emplace(-depth[v], seq++, v);
            }
      }
      bool complete = order.size() == n;
      for (int i = 0; i < n; i++) if (!done[i]) order.push_back(i);
      return writable::list({"order"_nm = order, "complete"_nm = complete});
}
