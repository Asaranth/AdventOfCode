# Advent of Code 2024 — Solution Notes

Each day is its own section with brief notes per part. Titles are placeholders so official AoC names can be filled later.

### Day 01 - [Title TBD]

#### Part 1
- Technique: sort both lists and sum absolute pairwise differences.
- What/Why: Align sorted lists index-wise to minimize total distance and compute the sum.

#### Part 2
- Technique: hashmap frequency aggregation (similarity score).
- What/Why: Count occurrences of right list values; for each left value add `value * freq[value]`.

### Day 02 - [Title TBD]

#### Part 1
- Technique: monotonicity check with bounded adjacent differences.
- What/Why: Validate that each report is either nondecreasing or nonincreasing and that step sizes are within [1,3].

#### Part 2
- Technique: single-removal robustness.
- What/Why: If a report fails, try removing one element and re-check the same constraints.

### Day 03 - [Title TBD]

#### Part 1
- Technique: parsing with stateful scanning.
- What/Why: Scan the input for instruction-like tokens, extracting and summing values per rules.

#### Part 2
- Technique: mode toggling while scanning.
- What/Why: Maintain an enabled/disabled state that gates whether tokens contribute to the sum.

### Day 04 - [Title TBD]

#### Part 1
- Technique: grid scanning in 8 directions.
- What/Why: Count occurrences of a target string along all ray directions for each cell.

#### Part 2
- Technique: pattern matching with crossing shapes.
- What/Why: Detect specific letter arrangements (e.g., X-shaped crosses) per puzzle specification.

### Day 05 - [Title TBD]

#### Part 1
- Technique: ordering constraints with validation.
- What/Why: For each sequence, verify that all required precedences are respected.

#### Part 2
- Technique: minimal reordering to satisfy constraints.
- What/Why: Repair invalid sequences by reordering to meet the partial order, then score.

### Day 06 - [Title TBD]

#### Part 1
- Technique: grid simulation of movement rules.
- What/Why: Move an agent across the grid applying turns on obstacles; count visited.

#### Part 2
- Technique: loop detection/backtracking.
- What/Why: Try placing a single obstacle to create a loop; detect cycles in the movement path.

### Day 07 - [Title TBD]

#### Part 1
- Technique: combinatorial search over operators.
- What/Why: Try assignments of +, *, concat (per puzzle) to reach target sums.

#### Part 2
- Technique: expanded operator set / additional constraint.
- What/Why: Extend the search with the extra operator or rule and recompute counts.

### Day 08 - [Title TBD]

#### Part 1
- Technique: geometry on a grid with vector projections.
- What/Why: Compute antenna resonance/antinode placements via reflections or vector alignment rules.

#### Part 2
- Technique: infinite resonance lines consideration.
- What/Why: Extend antinodes along full lines with step multiples; count within bounds.

### Day 09 - [Title TBD]

#### Part 1
- Technique: two-pointer compaction; checksum computation.
- What/Why: Collapse files to the left into empty slots while preserving relative order, then sum `index * file_id`.

#### Part 2
- Technique: first-fit segment search for file moves.
- What/Why: For each file (from highest id down), find the earliest gap large enough to move it whole; recompute checksum.

### Day 10 - [Title TBD]

#### Part 1
- Technique: BFS/DFS path counting on a height grid.
- What/Why: Count reachable trailheads/peaks under ascent constraints.

#### Part 2
- Technique: count distinct paths.
- What/Why: Accumulate path counts to destinations rather than mere reachability.

### Day 11 - [Title TBD]

#### Part 1
- Technique: memoization/DP over value transforms.
- What/Why: Apply the blink rules for a given number of steps with caching.

#### Part 2
- Technique: count compression.
- What/Why: Track counts of identical values to scale up to large step counts efficiently.

### Day 12 - [Title TBD]

#### Part 1
- Technique: region flood-fill with perimeter counting.
- What/Why: For each labeled region, compute area and boundary contribution.

#### Part 2
- Technique: edge/corner-aware perimeter.
- What/Why: Adjust counting to treat shared edges and corners per spec.

### Day 13 - [Title TBD]

#### Part 1
- Technique: linear system solving under constraints.
- What/Why: Find minimal cost button press combinations that reach target coordinates.

#### Part 2
- Technique: offset adjustment / bigger targets.
- What/Why: Re-solve after applying large offsets to targets, using math shortcuts.

### Day 14 - [Title TBD]

#### Part 1
- Technique: toroidal motion simulation.
- What/Why: Advance positions modulo width/height; compute quadrant counts.

#### Part 2
- Technique: density/cluster detection over time.
- What/Why: Search for a time step with recognizable pattern/low entropy.

### Day 15 - [Title TBD]

#### Part 1
- Technique: grid pushing rules.
- What/Why: Move a robot pushing boxes one cell at a time with collision rules.

#### Part 2
- Technique: widened boxes / different geometry.
- What/Why: Adapt pushing to 2-cell-wide boxes and recompute final score.

### Day 16 - [Title TBD]

#### Part 1
- Technique: Dijkstra/A* on a weighted grid with turn costs.
- What/Why: Minimize total movement plus turning penalties to reach the goal.

#### Part 2
- Technique: count optimal paths / backtracking over predecessors.
- What/Why: After distances known, count tiles that lie on any shortest path.

### Day 17 - [Title TBD]

#### Part 1
- Technique: VM/interpreter simulation with bitwise ops.
- What/Why: Run the instruction set to produce an output sequence.

#### Part 2
- Technique: search for input satisfying an output pattern.
- What/Why: Reverse-engineer or brute-force with pruning to match target output.

### Day 18 - [Title TBD]

#### Part 1
- Technique: BFS pathfinding under incremental obstacles.
- What/Why: Determine reachability as obstacles appear over time.

#### Part 2
- Technique: binary search on time threshold.
- What/Why: Find the last time when the target is still reachable.

### Day 19 - [Title TBD]

#### Part 1
- Technique: DP over string decompositions.
- What/Why: Count ways to compose a target string from pieces using memoization.

#### Part 2
- Technique: extended counting/constraints.
- What/Why: Adjust DP to count under modified constraints (e.g., limit or expand pieces).

### Day 20 - [Title TBD]

#### Part 1
- Technique: graph distances and shortcut analysis.
- What/Why: Compute baseline shortest paths and evaluate savings from proposed shortcuts.

#### Part 2
- Technique: meet-in-the-middle for savings tally.
- What/Why: Combine precomputed distances to count all shortcuts achieving a minimum saving.

### Day 21 - [Title TBD]

#### Part 1
- Technique: keypad as a graph; compose shortest sequences.
- What/Why: Build shortest instruction strings for layered keypads.

#### Part 2
- Technique: heavy memoization over states.
- What/Why: Cache subproblems for nested keypads to scale to long inputs.

### Day 22 - [Title TBD]

#### Part 1
- Technique: PRNG simulation; hashing.
- What/Why: Generate sequences and compute aggregate measures.

#### Part 2
- Technique: sliding-window sum maximization.
- What/Why: Track best-scoring patterns across windows using maps.

### Day 23 - [Title TBD]

#### Part 1
- Technique: clique detection / graph search.
- What/Why: Find maximal cliques or specific groupings per rules.

#### Part 2
- Technique: pruning and backtracking.
- What/Why: Use heuristics to search larger solution spaces efficiently.

### Day 24 - [Title TBD]

#### Part 1
- Technique: boolean circuit evaluation.
- What/Why: Propagate signals through gates to compute outputs; identify mismatches.

#### Part 2
- Technique: SAT-like reasoning; limited exhaustive search.
- What/Why: Swap/repair gates to satisfy expected outputs using search with pruning.

### Day 25 - [Title TBD]

#### Solution
- Technique: set compatibility checks; pairwise matching.
- What/Why: Count lock/key pairs that interlock without conflicts.
