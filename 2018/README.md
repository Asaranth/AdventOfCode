# Advent of Code 2018

<img src="https://img.shields.io/badge/-JavaScript-f1e05a?style=for-the-badge&labelColor=2b2b2b&logo=javascript&logoColor=white" alt="JavaScript">
<img src="https://img.shields.io/badge/⭐-50%2F50%20-009900?style=for-the-badge&labelColor=2b2b2b" alt="Stars">

### Day 01 – Chronal Calibration
#### Part 1
- **Requirement:** Sum all frequency changes to get the final frequency.
- **Technique:** Running a sum over signed changes.

#### Part 2
- **Requirement:** Find the first frequency reached twice.
- **Technique:** Cyclic running sum with set for duplicates.

---

### Day 02 – Inventory Management System
#### Part 1
- **Requirement:** Count box IDs containing letters exactly 2 or 3 times; multiply counts.
- **Technique:** Letter frequency counts.

#### Part 2
- **Requirement:** Find the two IDs differing by exactly one character; return common letters.
- **Technique:** Hamming distance 1 comparison.

---

### Day 03 – No Matter How You Slice It
#### Part 1
- **Requirement:** Count grid cells covered by two or more fabric claims.
- **Technique:** Rectangle overlap on grid.

#### Part 2
- **Requirement:** Identify the only claim that does not overlap any other.
- **Technique:** Non-overlapping claim detection.

---

### Day 04 – Repose Record
#### Part 1
- **Requirement:** Determine the guard with most total sleep; multiply guard ID × minute asleep most often.
- **Technique:** Guard log sorting and histograms.

#### Part 2
- **Requirement:** Find the guard-minute combination with the highest frequency.
- **Technique:** Per-minute argmax across guards.

---

### Day 05 – Alchemical Reduction
#### Part 1
- **Requirement:** Fully react polymer string; cancel adjacent units differing only in case.
- **Technique:** Stack-based string reduction.

#### Part 2
- **Requirement:** Find shortest polymer length by removing all units of one type and reducing.
- **Technique:** Two-phase filtering per unit type.

---

### Day 06 – Chronal Coordinates
#### Part 1
- **Requirement:** Find largest finite area closest to a given coordinate.
- **Technique:** Manhattan distance Voronoi regions.

#### Part 2
- **Requirement:** Count grid cells where sum of distances to all coordinates is below threshold.
- **Technique:** Bounded distance region.

---

### Day 07 – The Sum of Its Parts
#### Part 1
- **Requirement:** Determine step order respecting dependencies; lexicographically smallest.
- **Technique:** Topological sort (Kahn’s algorithm).

#### Part 2
- **Requirement:** Simulate multiple workers with step durations; compute total completion time.
- **Technique:** Multi-worker simulation.

---

### Day 08 – Memory Maneuver
#### Part 1
- **Requirement:** Sum all metadata entries in a tree encoded as numbers.
- **Technique:** Tree parsing from a stream.

#### Part 2
- **Requirement:** Compute node values where metadata references child nodes.
- **Technique:** Node value evaluation.

---

### Day 09 – Marble Mania
#### Part 1
- **Requirement:** Play marble game; report highest score.
- **Technique:** Circular deque simulation.

#### Part 2
- **Requirement:** Efficiently simulate a large number of marbles.
- **Technique:** Optimized deque/linked structure.

---

### Day 10 – The Stars Align
#### Part 1
- **Requirement:** Advance points with velocities until a message appears; render a message.
- **Technique:** Point motion simulation.

#### Part 2
- **Requirement:** Report second when a message appears (minimal bounding box).
- **Technique:** Detect minimal area timing.

---

### Day 11 – Chronal Charge
#### Part 1
- **Requirement:** Find 3×3 square with the largest total power in grid.
- **Technique:** Summed-area table (integral image).

#### Part 2
- **Requirement:** Find a square of any size with maximum power.
- **Technique:** Variable-size summed-area search.

---

### Day 12 – Subterranean Sustainability
#### Part 1
- **Requirement:** Simulate plant growth in pots for fixed generations; sum indices with plants.
- **Technique:** 1D automaton with a sliding window.

#### Part 2
- **Requirement:** Extrapolate far-future sum after pattern stabilizes.
- **Technique:** Linear growth detection and projection.

---

### Day 13 – Mine Cart Madness
#### Part 1
- **Requirement:** Simulate carts on tracks; report first crash location.
- **Technique:** Cart simulation with turns/curves.

#### Part 2
- **Requirement:** Continue until one cart remains; report its position.
- **Technique:** Remove-on-crash simulation.

---

### Day 14 – Chocolate Charts
#### Part 1
- **Requirement:** Generate a scoreboard; return 10 recipes after the target number.
- **Technique:** Recipe scoreboard simulation.

#### Part 2
- **Requirement:** Find first occurrence of a target sequence in growing scoreboard.
- **Technique:** Substring search in stream.

---

### Day 15 – Beverage Bandits
#### Part 1
- **Requirement:** Simulate grid combat; report remaining units after all turns.
- **Technique:** BFS movement with targeting/attack rules.

#### Part 2
- **Requirement:** Find minimal elf attack power yielding zero elf deaths; report remaining units.
- **Technique:** Binary search over attack power.

---

### Day 16 – Chronal Classification
#### Part 1
- **Requirement:** Count how many samples behave like three or more opcodes.
- **Technique:** Opcode deduction from samples.

#### Part 2
- **Requirement:** Determine full opcode mapping; execute program.
- **Technique:** Constraint propagation.

---

### Day 17 – Reservoir Research
#### Part 1
- **Requirement:** Simulate water flow; count all water tiles.
- **Technique:** Water fill simulation.

#### Part 2
- **Requirement:** Count only retained (settled) water tiles.
- **Technique:** Containment detection.

---

### Day 18 – Settlers of The North Pole
#### Part 1
- **Requirement:** Simulate acre resource evolution; compute value after N minutes.
- **Technique:** Cellular automaton.

#### Part 2
- **Requirement:** Fast-forward to 1,000,000,000 minutes using cycle detection.
- **Technique:** Cycle detection.

---

### Day 19 – Go With The Flow
#### Part 1
- **Requirement:** Run VM with a bound instruction pointer; report final register.
- **Technique:** Instruction pointer VM simulation.

#### Part 2
- **Requirement:** Optimize slow loop by summing divisors of the target.
- **Technique:** Mathematical factorization.

---

### Day 20 – A Regular Map
#### Part 1
- **Requirement:** Build graph from regex; find the farthest room.
- **Technique:** BFS on room graph.

#### Part 2
- **Requirement:** Count rooms at least 1000 doors away.
- **Technique:** Threshold counting.

---

### Day 21 – Chronal Conversion
#### Part 1
- **Requirement:** Find the first value causing VM to halt earliest.
- **Technique:** VM analysis.

#### Part 2
- **Requirement:** Find last unique value before cycle repeats.
- **Technique:** Track register values to detect cycle.

---

### Day 22 – Mode Maze
#### Part 1
- **Requirement:** Compute total risk level for area of cave.
- **Technique:** Erosion/region type calculations.

#### Part 2
- **Requirement:** Find the shortest time to target with gear change constraints.
- **Technique:** Dijkstra with gear state.

---

### Day 23 – Experimental Emergency Hull Repair
#### Part 1
- **Requirement:** Count nanobots in range of the strongest bot.
- **Technique:** 3D Manhattan spheres.

#### Part 2
- **Requirement:** Find a point in range of most bots, minimal distance to origin.
- **Technique:** Branch-and-bound search.

---

### Day 24 – Immune System Simulator 20XX
#### Part 1
- **Requirement:** Simulate fight; report total remaining units.
- **Technique:** Battle simulation.

#### Part 2
- **Requirement:** Find minimal boost for immune victory; report remaining units.
- **Technique:** Binary search on boost.

---

### Day 25 – Four-Dimensional Adventure
#### Solution
- **Requirement:** Cluster points in 4D; count constellations.
- **Technique:** Union–Find for 4D clustering.
