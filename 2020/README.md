# Advent of Code 2020

<img src="https://img.shields.io/badge/-Java-ED8B00?style=for-the-badge&labelColor=2b2b2b&logo=openjdk" alt="Java"> <img src="https://img.shields.io/badge/⭐-14%2F50%20-990000?style=for-the-badge&labelColor=2b2b2b" alt="Stars">

### Day 01 – Report Repair
#### Part 1
- **Requirement:** Find two numbers in the input that sum to 2020; return their product.
- **Technique:** Recursive combination search with target reduction.

#### Part 2
- **Requirement:** Find three numbers in the input that sum to 2020; return their product.
- **Technique:** Recursive combination search with target reduction.

---

### Day 02 – Password Philosophy
#### Part 1
- **Requirement:** Count passwords where the given letter appears at least the minimum number of times and at most the maximum number of times.
- **Technique:** Parse policy records, count character occurrences with streams, and filter by inclusive min/max bounds.

#### Part 2
- **Requirement:** Count passwords where the given letter appears in exactly one of the two specified positions.
- **Technique:** Treat policy numbers as one-based positions, check both indexed characters, and use XOR logic to require exactly one match.

---

### Day 03 – Toboggan Trajectory
#### Part 1
- **Requirement:** Count trees encountered starting at top-left and following a slope of right 3, down 1 across a horizontally repeating map until reaching the bottom.
- **Technique:** 2D grid traversal using modular arithmetic for horizontal pattern repetition (`col % width`).

#### Part 2
- **Requirement:** Count trees encountered across five different slopes and find the product of all counts.
- **Technique:** Generalised slope traversal helper parameterised by `(right, down)` and 64-bit product reduction.

---

### Day 04 – Passport Processing
#### Part 1
- **Requirement:** Count passports that contain all required fields (`byr`, `iyr`, `eyr`, `hgt`, `hcl`, `ecl`, `pid`; `cid` is optional).
- **Technique:** Parse blank-line-separated passport records into key-value maps and filter for the presence of all required fields.

#### Part 2
- **Requirement:** Count passports where all required fields are present and satisfy specific format and range validation rules.
- **Technique:** Strict data validation using regular expressions and bounded range checks for each field.

---

### Day 05 – Binary Boarding
#### Part 1
- **Requirement:** Find the highest seat ID on a boarding pass.
- **Technique:** Decode binary space partitioning instructions (`F`/`B` for 7-bit row, `L`/`R` for 3-bit column) to calculate seat ID (`row * 8 + column`) and find the maximum ID.

#### Part 2
- **Requirement:** Find the missing seat ID whose neighbouring seat IDs (`ID - 1` and `ID + 1`) are present.
- **Technique:** Collect all seat IDs in a set, determine the min and max seat IDs, and identify the missing ID with both adjacent neighbours present.

---

### Day 06 – Custom Customs
#### Part 1
- **Requirement:** Count the number of questions to which anyone in each group answered "yes" and return the sum across all groups.
- **Technique:** Concatenate each group's passenger answer strings, extract distinct character counts using streams, and sum totals across groups.

#### Part 2
- **Requirement:** Count the number of questions to which everyone in each group answered "yes" and return the sum across all groups.
- **Technique:** Filter unique characters from the first person's answers where every person in the group answered "yes", and sum counts across groups.

---

### Day 07 – Handy Haversacks
#### Part 1
- **Requirement:** Count how many bag colours can eventually contain at least one `shiny gold` bag.
- **Technique:** Build an inverted containment map (reverse adjacency graph) and traverse it using breadth-first search (BFS) to find all unique ancestor bag colours.

#### Part 2
- **Requirement:** Count the total number of individual bags required inside a single `shiny gold` bag.
- **Technique:** Recursive depth-first traversal of the bag containment graph to calculate the cumulative sum and product of all nested bags.

---

### Day 08 – Handheld Halting
#### Part 1
- **Requirement:** Simulate the boot code instructions until an instruction is about to execute a second time; return the accumulator value.
- **Technique:**

#### Part 2
- **Requirement:** Fix the boot program by changing exactly one `jmp` to `nop` (or `nop` to `jmp`) so it terminates normally; return the accumulator value after termination.
- **Technique:**

---

### Day 09 – Encoding Error
#### Part 1
- **Requirement:** Find the first number in the sequence (after the 25-number preamble) that is not the sum of two of the previous 25 numbers.
- **Technique:**

#### Part 2
- **Requirement:** Find a contiguous range of at least two numbers that sum to the invalid number from Part 1; return the sum of the smallest and largest numbers in that range.
- **Technique:**

---

### Day 10 – Adapter Array
#### Part 1
- **Requirement:** Connect all adapters from the 0-jolt outlet to your device; return the product of the number of 1-jolt differences and 3-jolt differences.
- **Technique:**

#### Part 2
- **Requirement:** Calculate the total number of distinct valid arrangements of adapters that can connect the charging outlet to your device.
- **Technique:**

---

### Day 11 – Seating System
#### Part 1
- **Requirement:** Simulate the seating cellular automaton based on adjacent seats until equilibrium; return the total number of occupied seats.
- **Technique:**

#### Part 2
- **Requirement:** Simulate the seating automaton based on the first visible seat in each of the eight directions with an occupancy threshold of 5; return the total number of occupied seats.
- **Technique:**

---

### Day 12 – Rain Risk
#### Part 1
- **Requirement:** Follow the navigation instructions to move and turn the ship; return the Manhattan distance from the starting position.
- **Technique:**

#### Part 2
- **Requirement:** Follow the navigation instructions to move and rotate the waypoint and move the ship toward it; return the Manhattan distance from the starting position.
- **Technique:**

---

### Day 13 – Shuttle Search
#### Part 1
- **Requirement:** Find the earliest bus you can take given your earliest departure time; return the bus ID multiplied by the minutes you need to wait.
- **Technique:**

#### Part 2
- **Requirement:** Find the earliest timestamp such that each bus departs at an offset matching its index position in the list.
- **Technique:**

---

### Day 14 – Docking Data
#### Part 1
- **Requirement:** Execute the initialisation program where bitmasks modify the binary values written to memory addresses; return the sum of all values in memory.
- **Technique:**

#### Part 2
- **Requirement:** Execute the initialisation program where bitmasks apply to memory addresses with floating bits; return the sum of all values in memory.
- **Technique:**

---

### Day 15 – Rambunctious Recitation
#### Part 1
- **Requirement:** Play the memory game starting with the input list; determine the 2020th number spoken.
- **Technique:**

#### Part 2
- **Requirement:** Play the memory game starting with the input list; determine the 30000000th number spoken.
- **Technique:**

---

### Day 16 – Ticket Translation
#### Part 1
- **Requirement:** Identify all invalid field values across nearby tickets that do not match any field's valid ranges; return the sum of these invalid values (error rate).
- **Technique:**

#### Part 2
- **Requirement:** Discard invalid tickets, determine the correct field assignment for each index, and return the product of the six "departure" field values on your ticket.
- **Technique:**

---

### Day 17 – Conway Cubes
#### Part 1
- **Requirement:** Simulate a 3D Conway's Game of Life on pocket dimension hypercubes for six cycles; return the total number of active cubes.
- **Technique:**

#### Part 2
- **Requirement:** Simulate a 4D Conway's Game of Life (4D hypercubes) for six cycles; return the total number of active cubes.
- **Technique:**

---

### Day 18 – Operation Order
#### Part 1
- **Requirement:** Evaluate math expressions where addition and multiplication have equal precedence and evaluate left-to-right; return the sum of all expression results.
- **Technique:**

#### Part 2
- **Requirement:** Evaluate math expressions where addition has higher precedence than multiplication; return the sum of all expression results.
- **Technique:**

---

### Day 19 – Monster Messages
#### Part 1
- **Requirement:** Determine how many received messages completely match rule `0` based on the rule definitions.
- **Technique:**

#### Part 2
- **Requirement:** Update rules `8` and `11` to be recursive (`8: 42 | 42 8` and `11: 42 31 | 42 11 31`); count how many messages match rule `0`.
- **Technique:**

---

### Day 20 – Jurassic Jigsaw
#### Part 1
- **Requirement:** Reassemble image tiles by matching edge patterns; find the four corner tiles and return the product of their IDs.
- **Technique:**

#### Part 2
- **Requirement:** Reconstruct the complete image without tile borders, search for sea monster patterns across orientations, and count the remaining `#` habitats (water roughness).
- **Technique:**

---

### Day 21 – Allergen Assessment
#### Part 1
- **Requirement:** Identify ingredients that cannot contain any listed allergens; return how many times those ingredients appear across all food items.
- **Technique:**

#### Part 2
- **Requirement:** Determine the exact ingredient containing each allergen, sort ingredients alphabetically by allergen name, and return them as a comma-separated list.
- **Technique:**

---

### Day 22 – Crab Combat
#### Part 1
- **Requirement:** Simulate the card game Combat between two players until one player wins; calculate the winning player's score.
- **Technique:**

#### Part 2
- **Requirement:** Simulate Recursive Combat with recursive sub-games and infinite game prevention rules; calculate the winning player's score.
- **Technique:**

---

### Day 23 – Crab Cups
#### Part 1
- **Requirement:** Simulate 100 moves of the circular cup game with 9 cups; return the cup labels clockwise starting after cup 1.
- **Technique:**

#### Part 2
- **Requirement:** Simulate 10 million moves with one million cups; return the product of the two cup labels immediately clockwise of cup 1.
- **Technique:**

---

### Day 24 – Lobby Layout
#### Part 1
- **Requirement:** Follow hexagonal step directions from the origin to flip hexagonal floor tiles between white and black; return the number of black tiles.
- **Technique:**

#### Part 2
- **Requirement:** Simulate 100 days of hexagonal cellular automaton state transitions; return the total number of black tiles.
- **Technique:**

---

### Day 25 – Combo Breaker
#### Solution
- **Requirement:** Determine the secret loop sizes for the card and door public keys using modular arithmetic and calculate the resulting encryption key.
- **Technique:**