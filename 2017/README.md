# Advent of Code 2017

### Day 01 – Inverse Captcha
#### Part 1
- **Requirement:** Sum all digits in the input that match the next digit in the circular list.
- **Technique:** Circular adjacency check.

#### Part 2
- **Requirement:** Sum all digits matching the digit halfway around the list.
- **Technique:** Halfway-around matching.

---

### Day 02 – Corruption Checksum
#### Part 1
- **Requirement:** For each row of numbers, compute the difference between the largest and smallest values and sum the results.
- **Technique:** Row min/max checksum.

#### Part 2
- **Requirement:** For each row, find the only two numbers where one evenly divides the other; sum the quotients.
- **Technique:** Divisible pair search.

---

### Day 03 – Spiral Memory
#### Part 1
- **Requirement:** Compute Manhattan distance from the center to a given number in a spiral arrangement.
- **Technique:** Spiral coordinate math.

#### Part 2
- **Requirement:** Fill a spiral with each cell being the sum of adjacent cells until exceeding a target number; report first value above target.
- **Technique:** Neighbor-sum spiral.

---

### Day 04 – High-Entropy Passphrases
#### Part 1
- **Requirement:** Count passphrases with no duplicate words.
- **Technique:** Word uniqueness check.

#### Part 2
- **Requirement:** Count passphrases that contain no anagrams.
- **Technique:** Sorted-word anagram detection.

---

### Day 05 – A Maze of Twisty Trampolines, All Alike
#### Part 1
- **Requirement:** Follow jump offsets in a list, incrementing each visited offset by 1; count steps to exit.
- **Technique:** Offset simulation.

#### Part 2
- **Requirement:** Modify jumps: decrement offsets ≥3, increment otherwise; count steps to exit.
- **Technique:** Conditional increment/decrement.

---

### Day 06 – Memory Reallocation
#### Part 1
- **Requirement:** Redistribute memory blocks until a configuration repeats; count cycles until repeat.
- **Technique:** Reallocation with cycle detection.

#### Part 2
- **Requirement:** Determine size of the loop between first repeat and second occurrence.
- **Technique:** Loop length detection.

---

### Day 07 – Recursive Circus
#### Part 1
- **Requirement:** Identify the bottom program of the tower (not listed as a child of any other).
- **Technique:** Tower parsing/root detection.

#### Part 2
- **Requirement:** Find the program causing imbalance in tower weights and determine the correct weight.
- **Technique:** Subtree weight calculation.

---

### Day 08 – I Heard You Like Registers
#### Part 1
- **Requirement:** Execute conditional register operations and report the largest value in any register after completion.
- **Technique:** Register simulation.

#### Part 2
- **Requirement:** Track the highest value ever held in any register throughout execution.
- **Technique:** Max-ever tracking.

---

### Day 09 – Stream Processing
#### Part 1
- **Requirement:** Parse a character stream containing nested groups and garbage; compute total group score.
- **Technique:** Stream parsing/group scoring.

#### Part 2
- **Requirement:** Count all non-canceled characters within garbage sections.
- **Technique:** Garbage counting.

---

### Day 10 – Knot Hash
#### Part 1
- **Requirement:** Perform a sparse knot hash and multiply the first two numbers of the final list.
- **Technique:** Partial knot hash.

#### Part 2
- **Requirement:** Compute the full 256-element knot hash, densifying with XOR, and output as hex string.
- **Technique:** Full knot hash.

---

### Day 11 – Hex Ed
#### Part 1
- **Requirement:** Compute distance from origin in a hex grid following movement instructions.
- **Technique:** Hex grid (cube coordinates) distance.

#### Part 2
- **Requirement:** Track the farthest distance from the origin reached along the path.
- **Technique:** Max distance tracking.

---

### Day 12 – Digital Plumber
#### Part 1
- **Requirement:** Count all nodes connected (directly or indirectly) to node 0.
- **Technique:** BFS/DFS graph traversal.

#### Part 2
- **Requirement:** Count the total number of connected groups in the graph.
- **Technique:** Component counting.

---

### Day 13 – Packet Scanners
#### Part 1
- **Requirement:** Compute the total severity for getting caught in a firewall at depth `t = depth`.
- **Technique:** Modular arithmetic on scanner cycles.

#### Part 2
- **Requirement:** Determine the smallest delay to traverse the firewall without getting caught.
- **Technique:** Safe delay search.

---

### Day 14 – Disk Defragmentation
#### Part 1
- **Requirement:** Generate 128 knot hashes from input; count all bits set to 1.
- **Technique:** Hash → bit grid.

#### Part 2
- **Requirement:** Count all distinct connected regions of used squares.
- **Technique:** BFS/DFS region counting.

---

### Day 15 – Dueling Generators
#### Part 1
- **Requirement:** Generate pairs of pseudo-random numbers; count pairs with matching lower 16 bits.
- **Technique:** PRNG comparison.

#### Part 2
- **Requirement:** Apply multiple-of filtering on generators; count matches.
- **Technique:** Picky generator filtering.

---

### Day 16 – Permutation Promenade
#### Part 1
- **Requirement:** Simulate dance moves (spin, exchange, partner) on programs.
- **Technique:** Permutation simulation.

#### Part 2
- **Requirement:** Detect cycle period and fast-forward simulation to 1 billion iterations.
- **Technique:** Cycle detection.

---

### Day 17 – Spinlock
#### Part 1
- **Requirement:** Insert numbers into a circular buffer with given step size; report value after last insertion.
- **Technique:** Spinlock simulation.

#### Part 2
- **Requirement:** Track only the value after 0 for 50 million insertions efficiently.
- **Technique:** O(1) memory tracking.

---

### Day 18 – Duet
#### Part 1
- **Requirement:** Run single-program VM; recover last sound played before first `rcv` triggers.
- **Technique:** VM simulation.

#### Part 2
- **Requirement:** Simulate two programs sending/receiving; count sends until deadlock.
- **Technique:** Message queue simulation.

---

### Day 19 – A Series of Tubes
#### Part 1
- **Requirement:** Follow ASCII maze path, collecting letters in order.
- **Technique:** Maze traversal.

#### Part 2
- **Requirement:** Count steps taken along the path until exit.
- **Technique:** Step counting.

---

### Day 20 – Particle Swarm
#### Part 1
- **Requirement:** Identify the particle that stays closest to the origin in the long term (based on velocity/acceleration).
- **Technique:** Acceleration heuristic.

#### Part 2
- **Requirement:** Remove particles that collide at the same position over time; report remaining.
- **Technique:** Collision pruning.

---

### Day 21 – Fractal Art
#### Part 1
- **Requirement:** Iteratively enhance a pixel grid following pattern rules; count on pixels after N iterations.
- **Technique:** Fractal rule expansion.

#### Part 2
- **Requirement:** Extend iterations and optimize with caching/composition.
- **Technique:** Pattern caching/tiling optimization.

---

### Day 22 – Sporifica Virus
#### Part 1
- **Requirement:** Simulate virus bursts on grid with two states; count infections after 10k bursts.
- **Technique:** Grid state machine.

#### Part 2
- **Requirement:** Extend simulation with four states; count infections after 10M bursts.
- **Technique:** Extended state machine.

---

### Day 23 – Coprocessor Conflagration
#### Part 1
- **Requirement:** Execute assembly program; count `mul` instructions executed.
- **Technique:** Assembunny interpreter.

#### Part 2
- **Requirement:** Analyze loop behavior; compute final value using number theory instead of simulation.
- **Technique:** Number theory optimization.

---

### Day 24 – Electromagnetic Moat
#### Part 1
- **Requirement:** Build all valid bridges from components; report strongest.
- **Technique:** DFS/backtracking.

#### Part 2
- **Requirement:** Among longest bridges, report strongest.
- **Technique:** Longest-then-strongest selection.

---

### Day 25 – Clock Signal
#### Solution
- **Requirement:** Execute Turing-like program for given steps; compute checksum.
- **Technique:** Turing machine simulation.
