# Advent of Code 2024

<img src="https://img.shields.io/badge/-Rust-dea584?style=for-the-badge&labelColor=2b2b2b&logo=rust&logoColor=white" alt="Rust"> <img src="https://img.shields.io/badge/⭐-50%2F50%20-009900?style=for-the-badge&labelColor=2b2b2b" alt="Stars">

### Day 01 – Historian Hysteria
#### Part 1
- **Requirement:** Given two lists of location IDs (left list, right list), sort each list and compute the sum of absolute differences between paired IDs (smallest with smallest, etc.). :contentReference[oaicite:1]{index=1}
- **Technique:** Sort both lists; pair by sorted index; sum absolute differences

#### Part 2
- **Requirement:** Same as Part 1 but treat the right-hand list as a multiset (i.e. count frequencies); for each ID in the left list, add `value * frequency_in_right_list`. (Equivalent “frequency-map” distance computation) :contentReference[oaicite:2]{index=2}
- **Technique:** Frequency-map / hashmap aggregation

---

### Day 02 – Red-Nosed Reports
#### Part 1
- **Requirement:** For each report (sequence of numbers), validate that it is either non-decreasing or non-increasing, and that all adjacent differences are within [1, 3]. Count how many reports satisfy those constraints. (Based on your notes)
- **Technique:** Monotonicity check + bounded adjacent-difference check

#### Part 2
- **Requirement:** For any report that fails Part 1, allow removing exactly one element — then re-check the same constraints; count as valid if any single-removal makes the report valid. (Your “single-removal robustness” version)
- **Technique:** Try single-element removal, then the same monotonicity + bounds check

---

### Day 03 – Mull It Over
#### Part 1
- **Requirement:** Parse a sequence of instruction-like tokens from input, scan, and accumulate a value according to specified rules. (As per your notes)
- **Technique:** Stateful scanning / token parsing

#### Part 2
- **Requirement:** As Part 1, but maintain a mode (enabled/disabled) while scanning — only count token contributions when the mode is enabled. (Your “mode toggling” variant)
- **Technique:** Mode toggling while scanning

---

### Day 04 – Ceres Search
#### Part 1
- **Requirement:** Given a 2D grid of letters, search for occurrences of a target word (or string) in all 8 directions (horizontal, vertical, diagonal, both forward, and backward) from any starting cell.
- **Technique:** Grid scanning along multiple directional vectors

#### Part 2
- **Requirement:** Extend Part 1 by detecting more complex patterns (e.g. crossing shapes or overlapped occurrences) per puzzle specification.
- **Technique:** Pattern-matching of shaped arrangements on a grid

---

### Day 05 – Print Queue
#### Part 1
- **Requirement:** Given a set of precedent constraints (e.g. “page X must come before page Y if both appear”) and a collection of page-sequences (“updates”), check which sequences already respect all constraints; for each valid one, extract some value (e.g. middle page) and sum them. (From your notes)
- **Technique:** Partial-order validation on sequences

#### Part 2
- **Requirement:** For invalid sequences (violating some precedence), attempt to reorder them minimally (respecting all constraints) to create a valid sequence; then apply same scoring (middle page etc.) and sum across all.
- **Technique:** Topological sort / reordering under partial-order constraints

---

### Day 06 – Guard Gallivant
#### Part 1
- **Requirement:** Given a grid map where a guard starts at a position and moves according to rules (move forward if possible; else turn / adapt on obstacles), simulate guard movement until exiting map; count number of distinct positions visited (including start). (Your “grid simulation” description)
- **Technique:** Grid simulation with movement rules and obstacle handling

#### Part 2
- **Requirement:** Try placing a single additional obstacle (in any empty cell) to see if it causes the guard’s path to loop (never exit). For each candidate obstacle, simulate movement and detect loops; analyze which placements produce loops and count visited positions (or other statistic) accordingly.
- **Technique:** Simulation + loop-detection / backtracking over candidate obstacle placements

---

### Day 07 – Bridge Repair
#### Part 1
- **Requirement:** Given input lines representing equations: each line is a target value followed by a list of numbers. Determine which equations can be made true by placing either `+` or `*` between the numbers (operators evaluated strictly left-to-right, no operator precedence, numbers in given order). Sum the target values of all equations that can possibly be satisfied. :contentReference[oaicite:3]{index=3}
- **Technique:** Combinatorial search over operator assignments (left-to-right evaluation)

#### Part 2
- **Requirement:** Extend the operator set or constraints per puzzle instructions — your notes say “expanded operator set / additional constraint.” **TODO: write exact requirement when you know it**
- **Technique:** Expanded operator-set evaluation + constrained combinatorial search

---

### Day 08 – Resonant Collinearity
#### Part 1
- **Requirement:** (From your notes) Some geometry-based problem on a grid: given input, compute alignment or “resonance/antinode placements” via reflections or vector-based alignment rules. **TODO: refine once you translate puzzle spec into requirement**
- **Technique:** Grid geometry + vector projections / alignment detection

#### Part 2
- **Requirement:** Extend Part 1 to consider “infinite resonance lines” (or extended rays) — count how many such lines (or intersections) lie within given bounds. **TODO: exact summary when spec known**
- **Technique:** Infinite-line extension + bounded counting / detection

---

### Day 09 – Disk Fragmenter
#### Part 1
- **Requirement:** Given a list representing files on a disk, simulate compaction: repeatedly move files left into empty slots while preserving relative order, then compute a checksum (e.g. sum of `index * file_id`) of the resulting arrangement. (Based on your “two-pointer compaction; checksum computation”)
- **Technique:** Two-pointer compaction + checksum calculation

#### Part 2
- **Requirement:** After compaction, allow moving files by “first-fit segment search”: for each file (e.g. from highest ID downward), if there is a large enough contiguous gap to move it as a block, relocate it; then recompute the checksum. (Your notes)
- **Technique:** First-fit segment search for block moves + recompute checksum

---

### Day 10 – Hoof It
#### Part 1
- **Requirement:** Given a height-grid (or terrain grid) input, count how many distinct “trailheads/peaks” (or reachable peaks) are reachable under certain ascent constraints. (Your notes say “BFS/DFS path counting on a height grid”.) **TODO: exact requirement based on puzzle spec**
- **Technique:** BFS / DFS path counting with movement constraints on a grid

#### Part 2
- **Requirement:** Instead of simply reachability, count the total number of distinct valid paths to destinations under the ascent/movement constraints. (Your notes: count distinct paths, not just reachability)
- **Technique:** Path-counting / dynamic programming over grid paths

---

### Day 11 – Plutonian Pebbles
#### Part 1
- **Requirement:** Given an initial list of engraved-number stones, simulate a “blink” operation that transforms stones according to rules (e.g. if stone is 0 → becomes 1; if even-digit number → split into two stones; otherwise multiply by 2024). Then after a fixed number of blinks, output number (or count) of stones. :contentReference[oaicite:4]{index=4}
- **Technique:** Simulation + iterative list transformation

#### Part 2
- **Requirement:** Scale up the operation (many more blinks or larger inputs) — optimise by compressing equivalent states (e.g. group identical stones, track counts instead of individual stones). (Your “count compression” note)
- **Technique:** Memoization / DP + count compression

---

### Day 12 – Garden Groups
#### Part 1
- **Requirement:** Given a labelled grid with regions, compute for each region its area and perimeter (or boundary contribution), or count how many distinct regions satisfy certain area/perimeter constraints. (Your “region flood-fill with perimeter counting”)
- **Technique:** Flood-fill / region detection + perimeter / area computation

#### Part 2
- **Requirement:** As Part 1, but adjust for edge-sharing and corner-sharing between regions (i.e. treat shared edges/corners properly), possibly merging or splitting contributions accordingly. (Your “edge/corner-aware perimeter”)
- **Technique:** Modified flood-fill / region detection with shared-edge accounting

---

### Day 13 – Claw Contraption
#### Part 1
- **Requirement:** Solve a system of linear constraints (e.g. button-press combinations) to reach specific target coordinates or configurations. (Your note: “linear system solving under constraints”)
- **Technique:** Linear algebra / system solving under constraints

#### Part 2
- **Requirement:** Re-solve after applying large offsets or transformations (e.g. scaled inputs, modified targets) — possibly using optimisations or adapted math to avoid brute-forcing. (Your “offset adjustment / bigger inputs” note)
- **Technique:** Adapted linear system solving / optimized constraint evaluation

---

### Day 14 – Restroom Redoubt
#### Part 1
- **Requirement:** On a toroidal (wrap-around) grid, simulate motion (e.g. movement of agents or objects) modulo width/height, then compute some aggregated value (e.g. positions reached, quadrant counts, occupancy). (Your “toroidal motion simulation” note)
- **Technique:** Modular-grid simulation / wrap-around logic

#### Part 2
- **Requirement:** Over multiple simulation steps/time, detect clusters or density patterns; find a time step where a given pattern or low-entropy configuration emerges, or compute time until stable/periodic state. (Your “density/cluster detection over time” variant)
- **Technique:** Time-evolution simulation + cluster / density detection

---

### Day 15 – Warehouse Woes
#### Part 1
- **Requirement:** Given a grid representing a warehouse (boxes, empty spaces, obstacles), simulate box-pushing by a robot under collision/pushing rules; compute final configuration or score. (Your “grid pushing rules (box-pushing)” note)
- **Technique:** Grid simulation + collision / pushing logic

#### Part 2
- **Requirement:** Extend to wider or oddly shaped boxes (e.g. 2-cell wide), adapt pushing rules accordingly, and compute final score or configuration under new geometry constraints. (Your “adapt box geometry & recompute” note)
- **Technique:** Geometry-aware grid simulation + constraint adaptation

---

### Day 16 – Reindeer Maze
#### Part 1
- **Requirement:** Given a maze (grid) with possible “shortcuts”, compute the shortest-path (minimal cost) from start to end, considering both movement cost and cost for turns or shortcuts. (Your note: “graph distances + shortcut analysis”)
- **Technique:** Dijkstra / shortest-path on a weighted grid (turn / move costs)

#### Part 2
- **Requirement:** Count (or enumerate) all distinct paths that achieve the minimal cost (or satisfy certain constraints), possibly combining precomputed segments to find total paths meeting minimal cost. (Your “meet-in-the-middle” note)
- **Technique:** Path enumeration / meet-in-the-middle + dynamic programming or combinatorial counting

---

### Day 17 – Chronospatial Computer
#### Part 1
- **Requirement:** Simulate a custom virtual machine / interpreter: given a program (list of opcodes + operands), execute, according to the spec (opcode behaviors, register storage, I/O), and produce output. (Your “VM / interpreter simulation” note)
- **Technique:** VM / interpreter simulation with bitwise / register / opcode logic

#### Part 2
- **Requirement:** Search for inputs (or initial state) such that the VM produces a desired output pattern — likely using brute-force with pruning or optimized search. (Your “search for input satisfying output pattern” note)
- **Technique:** Brute-force search + pruning / optimised search

---

### Day 18 – RAM Run
#### Part 1
- **Requirement:** Given a maze where obstacles may appear over time (or change), determine the reachability of a target (or paths) under dynamic obstacles — e.g. find whether path exists at a given time or after changes. (Your “BFS under incremental obstacles” note)
- **Technique:** BFS / pathfinding on a dynamic grid (time-varying obstacles)

#### Part 2
- **Requirement:** Determine a threshold time when the target becomes unreachable (or reachable) by binary searching over time, or find minimal time satisfying reachability constraints. (Your “binary search on time threshold” note)
- **Technique:** Time-based binary search + repeated pathfinding / reachability checks

---

### Day 19 – Linen Layout
#### Part 1
- **Requirement:** Given a target string and a set of pieces (substrings), count the number of ways to decompose the target string into the pieces. (Your “DP over string decompositions” note)
- **Technique:** Dynamic Programming (memoization) over string composition possibilities

#### Part 2
- **Requirement:** Extend the counting under additional constraints (e.g. limits on piece usage, extra conditions), or count under modified rules. (Your “extended DP with constraints” note)
- **Technique:** Constrained DP / combinatorial counting

---

### Day 20 – Race Condition
#### Part 1
- **Requirement:** Given a map of a racetrack (grid) with start and end positions, compute the minimal time to reach the end when allowed to move up/down/left/right; also, you can cheat exactly once — pass through walls as if they were free for up to 2 time-units, then return to normal track. Find fastest possible time under these rules. :contentReference[oaicite:14]{index=14}
- **Technique:** Weighted BFS / search with one “cheat” state (wall-pass allowed once)

#### Part 2
- **Requirement:** Determine how many distinct “cheats” (start/ end cheat-positions) yield a time saving of at least a given threshold (e.g. 100 picoseconds), or perform a more advanced analysis of cheat outcomes. (Based on example in puzzle description) :contentReference[oaicite:15]{index=15}
- **Technique:** Path enumeration / search including cheat paths + filtering by time savings

---

### Day 21 – Keypad Conundrum
#### Part 1
- **Requirement:** Given a directional keypad layout (with possible no-button gaps) and a robot controlling a button-pushing arm that can move but not directly press specific buttons, compute the minimal sequence of movements that produce a target code (sequence of button labels) on the numeric keypad. Then compute “complexity” per code as (length of move sequence) × (numeric value of code), sum across codes. :contentReference[oaicite:17]{index=17}
- **Technique:** Graph search / the shortest path on keypad-graph; then arithmetic scoring

#### Part 2
- **Requirement:** (If Part 2 exists) Likely extend to multiple robots, multiple keypads, or alternate keypad layouts — adapt search accordingly. **TODO: fill once you know puzzle spec**
- **Technique:** Extended graph/search + adapted scoring

---

### Day 22 – Monkey Market
#### Part 1
- **Requirement:** Simulate a pseudo-random number generator (PRNG) producing sequences; hash or process generated values to compute some aggregate measure. (Your “PRNG simulation; hashing” note)
- **Technique:** PRNG simulation + hashing / aggregate calculation

#### Part 2
- **Requirement:** Use sliding-window (or segment-window) sum/maximum over the generated sequence (or processed values) to find maximal score or satisfying window conditions. (Your “sliding-window sum maximization” note)
- **Technique:** Sliding-window algorithm + optimization

---

### Day 23 – LAN Party
#### Part 1
- **Requirement:** Given a graph (e.g. social network or connection graph), detect cliques (fully connected subgraphs) under specified constraints; return some metric (size, count, sum) over cliques found. (Your “clique detection / graph search” note)
- **Technique:** Graph search / clique detection

#### Part 2
- **Requirement:** Perform a more extensive search/pruning to find larger or constrained cliques (or all cliques) under additional constraints; manage complexity via pruning / heuristics. (Your “pruning + backtracking” note)
- **Technique:** Backtracking + pruning / combinatorial search

---

### Day 24 – Crossed Wires
#### Part 1
- **Requirement:** Build a logic circuit from input specification; evaluate the circuit for certain input/output expectations (e.g. given sets of wires and keys/locks), determine which combinations satisfy constraints. (Your “boolean circuit evaluation / signal propagation” note)
- **Technique:** Boolean-circuit simulation / evaluation

#### Part 2
- **Requirement:** Under modified conditions (e.g. swapped wires or adjusted constraints), search over possible wiring configurations to satisfy target outputs (SAT-like problem), count or identify valid configurations. (Your “SAT-like reasoning / constrained exhaustive search” note)
- **Technique:** Constraint search + pruning / exhaustive or optimised search

---

### Day 25 – Code Chronicle
- **Requirement:** Given schematics of multiple “locks” and “keys” (each represented as columns with pin/key-heights), determine how many unique lock/key pairs fit together without overlapping (i.e. for every column, lock’s pin height + key’s protrusion height ≤ available space). Return the count of compatible lock/key pairs. :contentReference[oaicite:22]{index=22}
- **Technique:** Convert schematics to integer-height arrays; nested iteration (locks × keys) with per-column overlap checks; count valid pairs  

