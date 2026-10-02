# Advent of Code 2020

<img src="https://img.shields.io/badge/-Java-ED8B00?style=for-the-badge&labelColor=2b2b2b&logo=openjdk" alt="Java"> <img src="https://img.shields.io/badge/⭐-12%2F50%20-990000?style=for-the-badge&labelColor=2b2b2b" alt="Stars">

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