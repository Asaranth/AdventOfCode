# Advent of Code 2019 — Requirements & Techniques Overview

### Day 01 – The Tyranny of the Rocket Equation
#### Part 1
- **Requirement:** Compute fuel for each module as `floor(m/3) - 2`; sum all.
- **Technique:** Arithmetic, iterative reduction.

#### Part 2
- **Requirement:** Add fuel for fuel recursively until nonpositive; sum total.
- **Technique:** Recursive fuel calculation.

---

### Day 02 – 1202 Program Alarm
#### Part 1
- **Requirement:** Run Intcode program with opcodes 1,2,99; return final state.
- **Technique:** Intcode VM execution.

#### Part 2
- **Requirement:** Find noun/verb producing target output.
- **Technique:** Brute-force search over noun/verb.

---

### Day 03 – Crossed Wires
#### Part 1
- **Requirement:** Compute Manhattan distance of the closest intersection of two wire paths.
- **Technique:** Grid tracing and Manhattan geometry.

#### Part 2
- **Requirement:** Find intersection with minimal combined steps.
- **Technique:** Step-count distance calculation.

---

### Day 04 – Secure Container
#### Part 1
- **Requirement:** Count 6-digit numbers with nondecreasing digits and any double.
- **Technique:** Digit rules with monotonicity check.

#### Part 2
- **Requirement:** Require at least one exact pair of equal digits.
- **Technique:** Exact pair frequency check.

---

### Day 05 – Sunny with a Chance of Asteroids
#### Part 1
- **Requirement:** Run Intcode with input=1, supporting parameter modes, jumps, and comparisons.
- **Technique:** Intcode VM with extended opcodes.

#### Part 2
- **Requirement:** Run Intcode with input=5; verify correct output for all modes.
- **Technique:** Extended tests with parameter modes.

---

### Day 06 – Universal Orbit Map
#### Part 1
- **Requirement:** Count total direct and indirect orbits.
- **Technique:** Tree/orbit map with ancestor counting.

#### Part 2
- **Requirement:** Compute orbital transfers between YOU and SAN.
- **Technique:** LCA path distance.

---

### Day 07 – Amplification Circuit
#### Part 1
- **Requirement:** Maximise thruster signal with a series of amplifiers (phase permutations).
- **Technique:** Permutation search.

#### Part 2
- **Requirement:** Maximise thruster signal with a feedback loop of amplifiers.
- **Technique:** Feedback loop scheduling.

---

### Day 08 – Space Image Format
#### Part 1
- **Requirement:** Find a layer with the fewest zeros; multiply the count of 1s × 2s.
- **Technique:** Image layer counting.

#### Part 2
- **Requirement:** Render the final image by stacking layers respecting transparency.
- **Technique:** Layer compositing.

---

### Day 09 – Sensor Boost
#### Part 1
- **Requirement:** Run Intcode with relative base addressing; BOOST test mode.
- **Technique:** Intcode with relative base.

#### Part 2
- **Requirement:** Ensure memory and math support large values.
- **Technique:** Big-integer-safe operations.

---

### Day 10 – Monitoring Station
#### Part 1
- **Requirement:** Count visible asteroids from best location.
- **Technique:** Visibility detection and angle sorting.

#### Part 2
- **Requirement:** Vaporise asteroids in order; find specific target.
- **Technique:** Angle + distance sorting for laser sweep.

---

### Day 11 – Space Police
#### Part 1
- **Requirement:** Paint hull panels; count unique panels painted.
- **Technique:** Intcode robot with hash map grid.

#### Part 2
- **Requirement:** Start on white; output rendered registration identifier.
- **Technique:** Hull painting and rendering.

---

### Day 12 – The N-Body Problem
#### Part 1
- **Requirement:** Simulate moons for N steps; compute total energy.
- **Technique:** N-body simulation.

#### Part 2
- **Requirement:** Determine steps until system repeats.
- **Technique:** Axis cycles + LCM of periods.

---

### Day 13 – Care Package
#### Part 1
- **Requirement:** Run arcade game Intcode; count block tiles drawn.
- **Technique:** Intcode arcade simulation.

#### Part 2
- **Requirement:** Play automatically for the highest score.
- **Technique:** Autopilot heuristic.

---

### Day 14 – Space Stoichiometry
#### Part 1
- **Requirement:** Compute ORE required for 1 FUEL, accounting for surplus.
- **Technique:** Reaction graph + topological processing.

#### Part 2
- **Requirement:** Maximise FUEL with given ORE budget.
- **Technique:** Binary search on ORE.

---

### Day 15 – Oxygen System
#### Part 1
- **Requirement:** Explore grid; locate oxygen system.
- **Technique:** BFS mapping with Intcode droid.

#### Part 2
- **Requirement:** Compute time to fill an entire area with oxygen.
- **Technique:** BFS spreading from oxygen.

---

### Day 16 – Flawed Frequency Transmission
#### Part 1
- **Requirement:** Apply FFT-like phase transforms; read first 8 digits.
- **Technique:** Pattern-based transformations.

#### Part 2
- **Requirement:** Efficiently compute large repeated signal offset.
- **Technique:** Suffix-sum optimization.

---

### Day 17 – Set and Forget
#### Part 1
- **Requirement:** Compute sum of alignment parameters on scaffold intersections.
- **Technique:** Scaffold path analysis + VM output parsing.

#### Part 2
- **Requirement:** Compress a movement path into main routine + 3 functions; feed VM.
- **Technique:** Command sequence compression.

---

### Day 18 – Many-Worlds Interpretation
#### Part 1
- **Requirement:** Collect all keys in the shortest path.
- **Technique:** BFS with bitmask representing keys.

#### Part 2
- **Requirement:** Multi-robot variant for quadrants.
- **Technique:** Multi-source BFS with combined keys.

---

### Day 19 – Tractor Beam
#### Part 1
- **Requirement:** Count affected points in 50×50 area.
- **Technique:** Tractor beam sampling.

#### Part 2
- **Requirement:** Fit 100×100 square in beam; report top-left.
- **Technique:** Directed search along beam edge.

---

### Day 20 – Donut Maze
#### Part 1
- **Requirement:** Compute the shortest path through portal maze.
- **Technique:** BFS with portal labels.

#### Part 2
- **Requirement:** Shortest path with recursion levels.
- **Technique:** BFS with level tracking.

---

### Day 21 – Springdroid Adventure
#### Part 1
- **Requirement:** Write a logic program to safely walk using sensors.
- **Technique:** Boolean reasoning for spring droid.

#### Part 2
- **Requirement:** Extend the program for longer jumps.
- **Technique:** Advanced sensor logic.

---

### Day 22 – Slam Shuffle
#### Part 1
- **Requirement:** Model card shuffles as linear transformations mod deck size.
- **Technique:** Modular arithmetic / affine functions.

#### Part 2
- **Requirement:** Apply shuffle transform a large number of times.
- **Technique:** Exponentiation by squaring.

---

### Day 23 – Category Six
#### Part 1
- **Requirement:** Simulate NAT network; report first Y sent to address 255.
- **Technique:** NAT + packet queue simulation.

#### Part 2
- **Requirement:** Detect repeated Y value sent by NAT to address 0.
- **Technique:** Cycle detection.

---

### Day 24 – Planet of Discord
#### Part 1
- **Requirement:** Evolve 5×5 bug grid until repeat; compute biodiversity rating.
- **Technique:** Automaton with repeat detection.

#### Part 2
- **Requirement:** Recursive grid evolution; count total bugs.
- **Technique:** Multi-layer automaton.

---

### Day 25 – Cryostasis
#### Solution
- **Requirement:** Explore rooms, collect items, find passcode to escape.
- **Technique:** Text-adventure exploration and search.
