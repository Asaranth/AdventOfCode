# Advent of Code 2023 — Requirements & Techniques Overview

### Day 01 – Trebuchet?!
#### Part 1
- Requirement: For each input line, take the first numeric digit and the last numeric digit, combine them into a two‑digit number, and sum across all lines.
- Technique: Parsing + String Scanning

#### Part 2
- Requirement: As above — but digits may also appear spelled out (words). Recognize spelled‑out numbers and treat them equivalently; then again form two‑digit numbers from first + last digit and sum.
- Technique: Extended Scanning Rules

---

### Day 02 – Cube Conundrum
#### Part 1
- Requirement: Parse each record of colored‑cube draws; validate based on given constraints to determine which draws are plausible; sum IDs (or counts) of valid draws.
- Technique: Validation Under Constraints

#### Part 2
- Requirement: Under revised interpretation or constraints, re‑evaluate all records and compute the resulting score or count of valid games according to the new rules.
- Technique: Minimal Requirements Aggregation

---

### Day 03 – Gear Ratios
#### Part 1
- Requirement: Given a grid (“schematic”) of digits and symbols, sum all numbers adjacent (including diagonals) to at least one non‑`.` symbol.
- Technique: Grid Parsing with Adjacency Checks

#### Part 2
- Requirement: For each gear symbol (e.g. `*`), if there are exactly two adjacent numbers, multiply them (gear ratio) and sum across all gears.
- Technique: Gear Detection with Regex/String Processing

---

### Day 04 – Scratchcards
#### Part 1
- Requirement: For each scratchcard: compare “winning numbers” list vs “your numbers”, count matches; compute a score per card (e.g. with doubling per additional match), then sum across cards.
- Technique: Set Intersections for Scoring

#### Part 2
- Requirement: Account for extra card‑copies: winning cards may spawn additional copies depending on prior wins; propagate counts of copies and compute total resulting score/cards.
- Technique: Combinatorics Over Card Copies

---

### Day 05 – If You Give A Seed A Fertilizer
#### Part 1
- Requirement: Given seeds and a chain of interval‑to‑interval mappings (seed → soil → fertilizer → … → final “location”), map each seed through the pipeline and return the minimal resulting location.
- Technique: Range Mapping

#### Part 2
- Requirement: Seeds may be given as ranges; propagate ranges through the mapping pipeline (splitting/merging as needed), then compute minimal resulting location across all seeds/ranges.
- Technique: Interval Sweep/Merge

---

### Day 06 – Wait For It
#### Part 1
- Requirement: Given parameters (e.g. “hold time” → “distance” relation), count integer start‑times such that distance exceeds a given threshold — by solving a quadratic inequality.
- Technique: Math Derivation and Quadratic Bounds

#### Part 2
- Requirement: Same problem with larger or concatenated inputs (e.g. big numbers), requiring big‑integer–safe computations.
- Technique: Big‑Integer Friendly Computation

---

### Day 07 – Camel Cards
#### Part 1
- Requirement: Evaluate many “hands” (cards). For each, classify by frequency signature and card values; rank them; compute total winnings across all hands.
- Technique: Hand‑Ranking with Custom Comparator

#### Part 2
- Requirement: Add jokers/wildcards to hands; adapt ranking logic to account for wildcards; recompute total winnings accordingly.
- Technique: Jokers/Wildcards Adaptation

---

### Day 08 – Haunted Wasteland
#### Part 1
- Requirement: Traverse a graph defined by labeled nodes and a sequence of L/R instructions; start from a given node and follow instructions to a target; return number of steps taken.
- Technique: Graph Traversal with Instructions

#### Part 2
- Requirement: Considering many possible start nodes or repeated traversals, detect cycles (state repeats), compute cycle periods (e.g. using LCM), and determine when/if the target will be reached under synchronization constraints.
- Technique: Cycle Detection and LCM

---

### Day 09 – Mirage Maintenance
#### Part 1
- Requirement: Given a numeric sequence, repeatedly compute successive differences until the sequence becomes constant or zero; then use the final value(s) to predict next value(s) and sum or output as required.
- Technique: Sequence Differences and Extrapolation

#### Part 2
- Requirement: Apply a “left‑extrapolation” (reverse extrapolation) using the same difference-based logic to compute a value before the start of sequence; output that result.
- Technique: Left Extrapolation

---

### Day 10 – Pipe Maze
#### Part 1
- Requirement: Given a maze of pipe segments and a start point, traverse until reaching a loop or meeting point; compute path length (steps) or farthest reachable point.
- Technique: Pipe Graph Traversal

#### Part 2
- Requirement: Once a loop is formed, determine the enclosed interior region; compute its area (e.g. via scanline or polygon formulas) from the traced loop path.
- Technique: Area Calculation

---

### Day 11 – Cosmic Expansion
#### Part 1
- Requirement: Given a grid with galaxies (`#`) and empty space (`.`), expand the grid by doubling any row or column that originally had no galaxy; then compute the sum of Manhattan distances between every pair of galaxies. :contentReference[oaicite:1]{index=1}
- Technique: Coordinate Expansion; Manhattan Distance

#### Part 2
- Requirement: As part 1 — but expand empty rows/columns by a much larger factor (e.g. a million times), then sum all pairwise distances between galaxies. :contentReference[oaicite:2]{index=2}
- Technique: Large Expansion Factors

---

### Day 12 – Hot Springs
#### Part 1
- Requirement: Each input row describes a series of springs (states `.` = operational, `#` = damaged, `?` = unknown) plus a list of integers indicating lengths of contiguous damaged‑spring groups. Count how many assignments of `?` → `.` or `#` result in a configuration matching exactly the group lengths. :contentReference[oaicite:3]{index=3}
- Technique: DP with Memoization Over Patterns

#### Part 2
- Requirement: Before evaluating, “unfold” each input row by repeating the pattern multiple times (as per puzzle rules), then count valid configurations under the same constraints. :contentReference[oaicite:4]{index=4}
- Technique: Repeated Pattern Product

---

### Day 13 – Point of Incidence
#### Part 1
- Requirement: Given a grid representing ash (`.`) and rock (`#`), determine a perfect reflection (vertical or horizontal) that maps the grid onto itself. Sum a computed “score” based on positions of the reflection lines. :contentReference[oaicite:5]{index=5}
- Technique: Mirror Detection

#### Part 2
- Requirement: Allow exactly one cell mismatch (Hamming distance 1) in the reflection — find if the grid can still be reflected (with near‑symmetry) and compute the corresponding score. (Based on common “near‑mirror” extension for Part 2).
- Technique: One‑Cell Correction

---

### Day 14 – Parabolic Reflector Dish
#### Part 1
- Requirement: Given a 2D map of rocks: rounded rocks (`O`) and cube-shaped rocks (`#`), simulate a “tilt” that causes all rounded rocks to roll north as far as possible. Then compute the total “load” on the north support beam: for each rounded rock, the number of rows from rock to south edge (inclusive). :contentReference[oaicite:6]{index=6}
- Technique: Simulation with Rolls

#### Part 2
- Requirement: Instead of a small map, simulate repeated tilts (or many rocks) — detect repeating states (cycles) and use cycle length to “jump ahead” (rather than simulating every tilt) to compute final load/height/score. (Typical Part 2 scaling trick.)
- Technique: Cycle Detection with Caching

---

### Day 15 – Lens Library
#### Part 1
- Requirement: Input is a list of comma‑separated labels. Hash each label (custom hash), sum all hashes to get result. (As described in your original notes.)
- Technique: Custom Hash Function

#### Part 2
- Requirement: Use a data structure (e.g. hashmap) representing “boxes” holding lenses. Simulate placing/removing lenses per puzzle rules; compute total focusing power or sum of lens properties accordingly. (As per your original notes.)
- Technique: Hashmap of Boxes; Simulation

---

### Day 16 – The Floor Will Be Lava
#### Part 1
- Requirement: Given a network (grid/graph) of mirrors/splitters and a set of beams entering from edges, simulate beam propagation; count how many tiles become “energized” (or lit) by beams. (As per your original notes.)
- Technique: Beam Simulation with Visited‑State Caching

#### Part 2
- Requirement: Try all possible entrance edges for beams; compute maximum energized tile count across all entrance choices; output that best result.
- Technique: Best‑of‑All‑Entrances Search

---

### Day 17 – Clumsy Crucible
#### Part 1
- Requirement: In a graph or grid, find a shortest path from start to goal under constraints: you can move in straight runs but run-length is limited (min/max run length before turn). Compute minimal cost/path length. (As per your original notes.)
- Technique: Constrained Dijkstra

#### Part 2
- Requirement: Same as Part 1 but with altered constraints (different min/max run-lengths etc.); recompute shortest path under new rules.
- Technique: Altered Constraints

---

### Day 18 – Lavaduct Lagoon
#### Part 1
- Requirement: Given a polygonal boundary (via dig instructions), compute enclosed area (number of tiles) — using polygon area method (e.g. shoelace) or lattice‑point algorithm (e.g. Pick’s theorem). (As per your original notes.)
- Technique: Polygon Area via Shoelace; Pick’s Theorem

#### Part 2
- Requirement: Similar but with steps given in a different base (e.g. hex or scaled steps) — apply coordinate/base conversion, then compute area/perimeter accordingly. (As per your original notes.)
- Technique: Large Steps in a Different Base

---

### Day 19 – Aplenty
#### Part 1
- Requirement: Given routing/workflow rules for parts, parse each input and route parts through the workflow to accept or reject; sum properties (e.g. counts or valid parts) as required. (As per your original notes.)
- Technique: Rule Parsing + Evaluation

#### Part 2
- Requirement: Generalize to multi‑dimensional/range inputs (e.g. hyper‑rectangles); recursively split ranges by thresholds and count all accepted combinations; output total count.
- Technique: Range Splitting with Recursion

---

### Day 20 – Pulse Propagation
#### Part 1
- Requirement: Given a network of logic nodes (broadcasters, flip‑flops, conjunctions) and a sequence of button‑press inputs, simulate signal propagation and count resulting pulses or outputs after one sequence. (As per your original notes.)
- Technique: Signal Propagation on a Graph

#### Part 2
- Requirement: Analyze repeating behavior / cycles in the network output; compute synchronization time or repeated output via cycle detection and least common multiple (LCM) of periods.
- Technique: Cycle/LCM Analysis

---

### Day 21 – Step Counter
#### Part 1
- Requirement: Given a grid and a number N, count how many distinct cells are reachable from the start after exactly (or up to) N steps (grid BFS). (As per your original notes.)
- Technique: BFS on a Grid with Steps

#### Part 2
- Requirement: For a large or infinite tiled grid with periodic structure, use periodicity and quadratic interpolation (or formula) to extrapolate how many cells can be reached for large N. (As per your original notes.)
- Technique: Periodicity and Quadratic Interpolation

---

### Day 22 – Sand Slabs
#### Part 1
- Requirement: Given a list of 3D bricks (each defined by two endpoints on a grid), simulate all bricks falling straight down until they rest on the ground or on other bricks. Determine which bricks can be safely disintegrated: a brick is safe if removing it would not cause any other brick to fall further.
- Technique: 3D stacking simulation + dependency graph for brick support

#### Part 2
- Requirement: For each brick, determine the number of other bricks that would fall if it were disintegrated (i.e., simulate a chain reaction). Compute the sum of all these counts across all bricks.
- Technique: Dependency graph simulation + cascading fall computation

---

### Day 23 – A Long Walk
#### Part 1
- Requirement: Given a grid with paths (.), forests (#), and slope tiles (^, >, v, <), compute the longest possible hike from the top-row path to the bottom-row path. Movement onto a slope must follow the arrow direction, and each tile can be visited at most once.
- Technique: Grid DFS with Directional Constraints

#### Part 2
- Requirement: As Part 1, but slopes are now treated as normal paths. Compute the longest hike without revisiting any tile.
- Technique: Grid DFS / Backtracking

---

### Day 24 – Never Tell Me The Odds
#### Part 1
- Requirement: Given a list of hailstones with 3D positions and velocities, consider only the X and Y axes. Count all pairs of hailstones whose future paths will intersect within a given test area.
- Technique: 3D Line/Plane Intersections

#### Part 2
- Requirement: Determine a single rock’s starting position and velocity so that it collides with every hailstone at some integer time. Return the sum of the X, Y, and Z coordinates of the rock’s initial position.
- Technique: Linear Algebra Search

---

### Day 25 - Snowverload
#### Solution
- Requirement: Disconnect exactly three wires to split the connected components into two separate groups; multiply the sizes of the resulting groups.
- Technique: Graph min-cut / edge connectivity.

