# Advent of Code 2025

<img src="https://img.shields.io/badge/-Haskell-5e5086?style=for-the-badge&labelColor=2b2b2b&logo=haskell&logoColor=white" alt="Haskell">
<img src="https://img.shields.io/badge/⭐-20%2F24%20-409900?style=for-the-badge&labelColor=2b2b2b" alt="Stars">

### Day 01 – Secret Entrance

#### Part 1
- **Requirement:** Simulate movement around a circular 100-position dial using instructions like `Lk` and `Rk`; count how many times the dial lands exactly on position 0.
- **Technique:** Circular track simulation using modular arithmetic.

#### Part 2
- **Requirement:** Without stepwise simulation, compute how many times movement instructions cause the dial to cross position 0.
- **Technique:** Integer-division delta analysis to count wrap-around crossings.

---

### Day 02 – Gift Shop

#### Part 1
- **Requirement:** Expand numeric ranges into individual IDs; count how many IDs have even digit-length and whose first half matches the second half.
- **Technique:** Range parsing; half-string equality check.

#### Part 2
- **Requirement:** Determine whether each ID’s decimal representation is a repeated‐pattern string (periodic).
- **Technique:** Repetition detection using `(s + s)` substring method.

---

### Day 03 – Lobby

#### Part 1
- **Requirement:** From each bank of digits, select two digits (preserving order) that produce the largest possible 2-digit number.
- **Technique:** Greedy windowed selection.

#### Part 2
- **Requirement:** Build the lexicographically largest 12-digit number by selecting digits under the same greedy-selection rules.
- **Technique:** Greedy construction scaled to large output.

---

### Day 04 – Printing Department

#### Part 1
- **Requirement:** Count all `@` cells that have fewer than four occupied neighbours in the 8-direction neighbourhood.
- **Technique:** Grid neighbor-degree check.

#### Part 2
- **Requirement:** Iteratively remove all `@` cells with degree < 4 until no more qualify; total all removed.
- **Technique:** Iterative pruning (“peeling”) by degree.

---

### Day 05 – Cafeteria

#### Part 1
- **Requirement:** Given ranges of “fresh” IDs and a list of candidate IDs, count how many candidates fall in at least one fresh range.
- **Technique:** Interval containment checks.

#### Part 2
- **Requirement:** Merge overlapping or adjacent fresh ranges and sum the total covered span.
- **Technique:** Interval merge + length summation.

---

### Day 06 – Column Operatinator

#### Part 1
- **Requirement:** Treat each column as digits followed by an operator (`+` or `*`); aggregate the digits using that operator.
- **Technique:** Columnar transpose and operator-driven reduction.

#### Part 2
- **Requirement:** Parse columns from right to left, bundling digits into numbers until an operator column is found, then aggregate and reset.
- **Technique:** Right-to-left streaming accumulation.

---

### Day 07 – Laser Splittinator

#### Part 1
- **Requirement:** Simulate downward-moving beams in a grid; when encountering `^`, split beams left and right. Count total splits.
- **Technique:** Scanline beam simulation with deduped beam tracking.

#### Part 2
- **Requirement:** Track number of beams per column as they travel; at each `^`, double to neighbours. Sum total beams exiting the bottom.
- **Technique:** Dynamic programming over columns using vector propagation.

---

### Day 08 – Playground

#### Part 1
- **Requirement:** Compute distances between all point pairs; union the 1000 shortest edges to form components. Return the product of the three largest component sizes.
- **Technique:** Kruskal-like DSU using the 1000 nearest edges.

#### Part 2
- **Requirement:** Union edges in increasing distance order until all points are connected. For the final union, output is the product of the two points’ x-coordinates.
- **Technique:** Full DSU edge sweep until final merge.

---

### Day 09 – Movie Theater

#### Part 1
- **Requirement:** For all pairs of given red grid points, form axis-aligned rectangles using them as opposite corners; return the maximum rectangle area.
- **Technique:** Combinatorial rectangle search.

#### Part 2
- **Requirement:** Use the red points to define a rectilinear polygon; consider only rectangles entirely inside the polygon. Return the largest such rectangle area.
- **Technique:** Rectangle–polyline AABB collision checks.

---

### Day 10 – Factory

#### Part 1
- **Requirement:** Determine the minimum number of button presses needed to match all machines’ indicator light diagrams.
- **Technique:** BFS over XOR bitmask states to find minimal button presses.

#### Part 2
- **Requirement:** Determine the fewest total button presses needed to configure all machines’ "joltage" counters to exactly match their specified requirements, taking into account that buttons can affect multiple counters simultaneously.
- **Technique:** Backtracking search with pruning and bitmask arithmetic; each machine is modelled as a system of linear constraints, and the solver explores combinations of button presses to minimise the total count while satisfying all constraints.

---

### Day 11 – [Title TBD]

#### Part 1
- **Requirement:** TBD
- **Technique:** TBD

#### Part 2
- **Requirement:** TBD
- **Technique:** TBD

---

### Day 12 – [Title TBD]

#### Part 1
- **Requirement:** TBD
- **Technique:** TBD

#### Part 2
- **Requirement:** TBD
- **Technique:** TBD
