# Advent of Code 2020

<img src="https://img.shields.io/badge/-Java-ED8B00?style=for-the-badge&labelColor=2b2b2b&logo=openjdk" alt="Java"> <img src="https://img.shields.io/badge/⭐-04%2F50%20-990000?style=for-the-badge&labelColor=2b2b2b" alt="Stars">

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