# Advent of Code 2016

### Day 01 – No Time for a Taxicab
#### Part 1
- **Requirement:** Follow a sequence of turn-and-walk instructions, tracking position on a grid; compute Manhattan distance from the starting point.
- **Technique:** Grid walking with orientation.

#### Part 2
- **Requirement:** Track every intermediate step and identify the first location visited twice.
- **Technique:** Visited-set tracking.

---

### Day 02 – Bathroom Security
#### Part 1
- **Requirement:** Move across a standard 3×3 keypad using U/D/L/R instructions to determine each digit of a bathroom code.
- **Technique:** Keypad navigation with bounds.

#### Part 2
- **Requirement:** Use an irregular diamond keypad and repeat the navigation rules to extract a second code.
- **Technique:** Custom keypad adjacency rules.

---

### Day 03 – Squares With Three Sides
#### Part 1
- **Requirement:** For each input line, check whether the three values can form a valid triangle.
- **Technique:** Triangle inequality checks.

#### Part 2
- **Requirement:** Instead of reading triangles across rows, read them vertically in groups of three rows.
- **Technique:** Column-wise regrouping.

---

### Day 04 – Security Through Obscurity
#### Part 1
- **Requirement:** Determine which room names are “real” by computing a checksum from letter frequencies; sum sector IDs of valid rooms.
- **Technique:** Frequency counting + sorted checksum.

#### Part 2
- **Requirement:** Decrypt real room names using Caesar shifts by sector ID to find the one that mentions “northpole object storage”.
- **Technique:** Caesar rotation.

---

### Day 05 – How About a Nice Game of Chess?
#### Part 1
- **Requirement:** Generate MD5 hashes of door ID + integer; collect characters from hashes beginning with `00000` until an 8-char password forms.
- **Technique:** MD5 brute force.

#### Part 2
- **Requirement:** Use the 6th hex digit of valid hashes as a position, and the 7th as the character; fill password positions until complete.
- **Technique:** Positional MD5 extraction.

---

### Day 06 – Signals and Noise
#### Part 1
- **Requirement:** For each column of characters, determine the most common letter to recover the original message.
- **Technique:** Column frequency analysis.

#### Part 2
- **Requirement:** Instead of most common, determine the least common letter per column.
- **Technique:** Inverted frequency selection.

---

### Day 07 – Internet Protocol Version 7
#### Part 1
- **Requirement:** Determine if an address supports TLS by checking for ABBA patterns outside brackets and ensuring none appear inside.
- **Technique:** ABBA pattern scanning.

#### Part 2
- **Requirement:** Determine SSL support by matching ABA sequences outside with corresponding BAB sequences inside brackets.
- **Technique:** ABA/BAB pairing.

---

### Day 08 – Two-Factor Authentication
#### Part 1
- **Requirement:** Simulate a 50×6 pixel screen and apply rectangle and rotation operations; count lit pixels.
- **Technique:** Grid simulation.

#### Part 2
- **Requirement:** Render the final screen state and read the displayed letters.
- **Technique:** Same simulation; visual decoding.

---

### Day 09 – Explosives in Cyberspace
#### Part 1
- **Requirement:** Compute the decompressed length of a string with `(AxB)` markers, treating markers non-recursively.
- **Technique:** Linear marker parsing.

#### Part 2
- **Requirement:** Compute decompressed length with markers applied recursively (potentially huge output).
- **Technique:** Recursive length evaluation.

---

### Day 10 – Balance Bots
#### Part 1
- **Requirement:** Simulate bots passing chips according to rules; identify the bot that compares two specific chip values.
- **Technique:** Directed simulation.

#### Part 2
- **Requirement:** After simulation stabilises, multiply the values in output bins 0, 1, and 2.
- **Technique:** Extract outputs post-simulation.

---

### Day 11 – Radioisotope Thermoelectric Generators
#### Part 1
- **Requirement:** Move microchips and generators between floors safely to reach the top; minimise moves.
- **Technique:** BFS/IDDFS with state canonicalization.

#### Part 2
- **Requirement:** Repeat with additional items added, requiring more steps.
- **Technique:** Same search on expanded state.

---

### Day 12 – Leonardo’s Monorail
#### Part 1
- **Requirement:** Run a simple "Assembunny" program and report register values at completion.
- **Technique:** Assembly interpreter.

#### Part 2
- **Requirement:** Re-run program with altered initial register (`c = 1`) and report the new result.
- **Technique:** Interpreter re-run.

---

### Day 13 – A Maze of Twisty Little Cubicles
#### Part 1
- **Requirement:** Use the given formula to determine wall/open spaces; find shortest path to target coordinate.
- **Technique:** BFS in generated maze.

#### Part 2
- **Requirement:** Count how many unique locations can be reached within 50 steps.
- **Technique:** BFS reachability.

---

### Day 14 – One-Time Pad
#### Part 1
- **Requirement:** Generate MD5 hashes, find triplet → quintuplet matches; identify the 64th key.
- **Technique:** MD5 hashing with sliding windows.

#### Part 2
- **Requirement:** Apply 2016 extra MD5 rehashes to each candidate hash before testing; again find the 64th key.
- **Technique:** Key-stretch MD5.

---

### Day 15 – Timing is Everything
#### Part 1
- **Requirement:** Determine earliest time to drop a capsule so it passes through multiple rotating discs exactly when each aligns.
- **Technique:** Modular arithmetic / congruences.

#### Part 2
- **Requirement:** Include an additional disc and recompute the earliest valid drop time.
- **Technique:** Expand congruence system.

---

### Day 16 – Dragon Checksum
#### Part 1
- **Requirement:** Generate data using dragon curve expansion until reaching target length; compute checksum.
- **Technique:** Efficient string growth + checksum folding.

#### Part 2
- **Requirement:** Repeat the same process for much larger data size.
- **Technique:** Optimized expansion.

---

### Day 17 – Two Steps Forward
#### Part 1
- **Requirement:** Move through a 4×4 maze where door states depend on MD5 of passcode+path; find shortest path to vault.
- **Technique:** BFS guided by MD5 door states.

#### Part 2
- **Requirement:** Find the *longest* path that still reaches the vault.
- **Technique:** DFS longest path search.

---

### Day 18 – Like a Rogue
#### Part 1
- **Requirement:** Generate tile rows using trap/safe rules and count safe tiles over 40 rows.
- **Technique:** 1D cellular automaton.

#### Part 2
- **Requirement:** Extend the process to 400,000 rows efficiently.
- **Technique:** Streaming row generation.

---

### Day 19 – An Elephant Named Joseph
#### Part 1
- **Requirement:** Determine the elf who wins the circle-stealing game (standard Josephus variant).
- **Technique:** Josephus bit trick.

#### Part 2
- **Requirement:** Determine the winning elf in the across-the-circle elimination variant.
- **Technique:** Mathematical recurrence / optimised simulation.

---

### Day 20 – Firewall Rules
#### Part 1
- **Requirement:** Merge ranges of blocked IPs and find the smallest allowed IP.
- **Technique:** Interval merging.

#### Part 2
- **Requirement:** Count how many IPs are allowed (not in any blocked interval).
- **Technique:** Gap size summation.

---

### Day 21 – Scrambled Letters and Hash
#### Part 1
- **Requirement:** Apply various scrambling operations (rotate, swap, reverse, move) to produce a scrambled password.
- **Technique:** Direct string manipulation.

#### Part 2
- **Requirement:** Reverse the process to find the original password from a scrambled one.
- **Technique:** Inverting operations.

---

### Day 22 – Grid Computing
#### Part 1
- **Requirement:** Count viable pairs of nodes where one node’s used data fits into another’s available space.
- **Technique:** Pairwise capacity evaluation.

#### Part 2
- **Requirement:** Move the goal data to position (0,0) with minimal steps, using the empty node as a sliding space.
- **Technique:** BFS movement planning.

---

### Day 23 – Safe Cracking
#### Part 1
- **Requirement:** Run "Assembunny" code with self-modifying `tgl` instruction; determine resulting register value.
- **Technique:** "Assembunny" interpreter + optimization.

#### Part 2
- **Requirement:** Run again with larger starting registers; compute final value.
- **Technique:** Mathematical shortcut (factorial + additions).

---

### Day 24 – Air Duct Spelunking
#### Part 1
- **Requirement:** Compute shortest route visiting all marked positions (0–n) at least once.
- **Technique:** BFS pairwise distances + TSP bitmask DP.

#### Part 2
- **Requirement:** Same as Part 1, but must return to the starting position.
- **Technique:** TSP DP with return constraint.

---

### Day 25 – Clock Signal
#### Solution
- **Requirement:** Find the smallest initial register value `a` that causes the "Assembunny" program to output an alternating 0/1 clock signal.
- **Technique:** "Assembunny" execution + pattern detection.
