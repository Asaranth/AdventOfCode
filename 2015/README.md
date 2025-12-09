# Advent of Code 2015

### Day 01 – Not Quite Lisp
#### Part 1
- **Requirement:** Process parentheses to compute final floor (up / down moves).
- **Technique:** Running sum / character scan.

#### Part 2
- **Requirement:** Find first position where cumulative floor reaches −1.
- **Technique:** First-passage detection / prefix-sum scan.

---

### Day 02 – I Was Told There Would Be No Math
#### Part 1
- **Requirement:** Compute wrapping paper needed: surface area + smallest-side area.
- **Technique:** Simple math formulas / min-face selection.

#### Part 2
- **Requirement:** Ribbon needed: smallest perimeter + volume.
- **Technique:** Sort dimensions / perimeter calculation.

---

### Day 03 – Perfectly Spherical Houses in a Vacuum
#### Part 1
- **Requirement:** Track all visited houses from movement instructions.
- **Technique:** 2D grid walk / set of visited coordinates.

#### Part 2
- **Requirement:** Santa and Robo-Santa alternate moves; count unique houses visited.
- **Technique:** Two pointers / alternating steps / set union.

---

### Day 04 – The Ideal Stocking Stuffer
#### Part 1
- **Requirement:** Smallest integer producing MD5 hash with 5 leading zeros.
- **Technique:** MD5 brute-force search.

#### Part 2
- **Requirement:** Same but with 6 leading zeros.
- **Technique:** Extended MD5 brute-force / prefix check.

---

### Day 05 – Doesn’t He Have Intern-Elves For This?
#### Part 1
- **Requirement:** Count strings with 3 vowels, double letter, and no banned substrings.
- **Technique:** Pattern checks / simple scanning.

#### Part 2
- **Requirement:** Count strings with repeating pair (non-overlapping) and a letter repeating with one between.
- **Technique:** Sliding windows / pattern detection.

---

### Day 06 – Probably a Fire Hazard
#### Part 1
- **Requirement:** Apply on/off/toggle operations to 1000×1000 grid; count lit lights.
- **Technique:** 2D boolean grid / range updates.

#### Part 2
- **Requirement:** Brightness grid with +1, +2, −1 rules; sum all brightness.
- **Technique:** 2D integer grid / bounded decrement.

---

### Day 07 – Some Assembly Required
#### Part 1
- **Requirement:** Evaluate wire signals from bitwise logic and dependencies.
- **Technique:** Recursive evaluation / memoization / parsing.

#### Part 2
- **Requirement:** Override wire `b` using Part 1 result; re-evaluate wire `a`.
- **Technique:** Memo reset / re-run evaluation.

---

### Day 08 – Matchsticks
#### Part 1
- **Requirement:** Compute code-length minus in-memory-length for strings with escape sequences.
- **Technique:** Escape parsing / string interpretation.

#### Part 2
- **Requirement:** Compute encoded-length minus original-length for re-escaped strings.
- **Technique:** String re-encoding rules.

---

### Day 09 – All in a Single Night
#### Part 1
- **Requirement:** Find shortest route visiting all cities exactly once.
- **Technique:** Permutations / brute-force TSP.

#### Part 2
- **Requirement:** Find the longest valid route.
- **Technique:** Same permutations / maximize path length.

---

### Day 10 – Elves Look, Elves Say
#### Part 1
- **Requirement:** Apply look-and-say transformation 40 times; output length.
- **Technique:** Run-length encoding (RLE).

#### Part 2
- **Requirement:** Same but 50 iterations.
- **Technique:** Optimized RLE / efficient concatenation.

---

### Day 11 – Corporate Policy
#### Part 1
- **Requirement:** Find next valid password meeting straight, banned-char, and pair rules.
- **Technique:** Increment base-26 string / validation checks.

#### Part 2
- **Requirement:** Find the next valid password after the first.
- **Technique:** Continue increment & validate.

---

### Day 12 – JSAbacusFramework.io
#### Part 1
- **Requirement:** Sum all numbers in JSON input.
- **Technique:** Recursive JSON traversal.

#### Part 2
- **Requirement:** Ignore any object containing `"red"` and sum the rest.
- **Technique:** Conditional recursion / branch pruning.

---

### Day 13 – Knights of the Dinner Table
#### Part 1
- **Requirement:** Maximize happiness around a circular seating arrangement.
- **Technique:** Permutations / adjacency scoring.

#### Part 2
- **Requirement:** Add self with zero-value edges; recompute best arrangement.
- **Technique:** Extend graph / rerun permutations.

---

### Day 14 – Reindeer Olympics
#### Part 1
- **Requirement:** Compute distances after fixed race time from run/rest cycles.
- **Technique:** Cycle math or second-by-second simulation.

#### Part 2
- **Requirement:** Award points per second to leading reindeer(s).
- **Technique:** Per-second simulation / leader tracking.

---

### Day 15 – Science for Hungry People
#### Part 1
- **Requirement:** Allocate 100 teaspoons to maximize cookie score.
- **Technique:** Brute-force combinations / nested loops.

#### Part 2
- **Requirement:** Only consider mixes with exactly 500 calories.
- **Technique:** Filtered brute-force.

---

### Day 16 – Aunt Sue
#### Part 1
- **Requirement:** Find which Sue matches known property values exactly.
- **Technique:** Property comparison / dictionary filtering.

#### Part 2
- **Requirement:** Use modified comparison rules: >, <, or = depending on property.
- **Technique:** Custom rule-based matching.

---

### Day 17 – No Such Thing as Too Much
#### Part 1
- **Requirement:** Count combinations of containers that exactly sum to 150 liters.
- **Technique:** Subset enumeration / DFS.

#### Part 2
- **Requirement:** Count only combinations using the fewest containers.
- **Technique:** Track minimal subset size / filter.

---

### Day 18 – Like a GIF For Your Yard
#### Part 1
- **Requirement:** Run cellular automaton for 100 steps; count lights on.
- **Technique:** Conway-style grid update.

#### Part 2
- **Requirement:** Keep four corners permanently on.
- **Technique:** Automaton + forced corner state.

---

### Day 19 – Medicine for Rudolph
#### Part 1
- **Requirement:** Apply each replacement once at every location; count distinct results.
- **Technique:** String substitution / unique-set tracking.

#### Part 2
- **Requirement:** Reduce molecule to `e` in fewest steps.
- **Technique:** Reverse greedy reductions / randomized descent.

---

### Day 20 – Infinite Elves and Infinite Houses
#### Part 1
- **Requirement:** Find first house with ≥ target presents (10× divisor sum).
- **Technique:** Divisor-sum sieve.

#### Part 2
- **Requirement:** Each elf visits only 50 houses; presents = 11× divisor sum.
- **Technique:** Modified sieve with visit cap.

---

### Day 21 – RPG Simulator 20XX
#### Part 1
- **Requirement:** Find cheapest gear set that defeats the boss.
- **Technique:** Brute-force equipment combinations / fight simulation.

#### Part 2
- **Requirement:** Find most expensive gear set that still loses.
- **Technique:** Same brute-force / maximize losing sets.

---

### Day 22 – Wizard Simulator 20XX
#### Part 1
- **Requirement:** Win fight with minimal mana using spells with effects & timers.
- **Technique:** DFS state search / pruning / memoization.

#### Part 2
- **Requirement:** Hard mode: player loses 1 HP each turn; minimize mana.
- **Technique:** Same DFS with extra constraint.

---

### Day 23 – Opening the Turing Lock
#### Part 1
- **Requirement:** Interpret assembly-like instructions until program halts.
- **Technique:** Instruction pointer simulation.

#### Part 2
- **Requirement:** Rerun with register `a = 1`.
- **Technique:** Same interpreter, different starting state.

---

### Day 24 – It Hangs in the Balance
#### Part 1
- **Requirement:** Split packages into two equal-weight groups; minimize group size then quantum entanglement.
- **Technique:** Subset generation / weight check / QE minimization.

#### Part 2
- **Requirement:** Split packages into three groups; same criteria.
- **Technique:** Subset generation for 3-way partitioning.

---

### Day 25 – Let It Snow
- **Requirement:** Identify code index in diagonal grid; compute correct code via iterative multiplication.
- **Technique:** Diagonal arithmetic / modular progression.
