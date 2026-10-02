---
sessionId: session-261002-125444-lg60
---

# Requirements

### Overview & Goals
Advent of Code 2020 Day 3 ("Toboggan Trajectory") requires navigating a 2D map of open squares (`.`) and trees (`#`) where the horizontal pattern repeats infinitely to the right. The goal is to update the repository's `2020/README.md` to document the requirements and solution techniques for Day 3, and establish the implementation approach for solving both Part 1 and Part 2 in `Day03.java`.

### Scope
#### In Scope
- Updating `2020/README.md` to document requirements and techniques for Day 3 Part 1 and Part 2 adhering to the repository format.
- Designing and implementing the solution in `2020/src/aoc2020/Day03.java`.
- Fixing the input data call in `Day03.run()` from `Utils.getInputData(2)` to `Utils.getInputData(3)`.

#### Out of Scope
- Modifying earlier days (`Day01.java`, `Day02.java`) or framework classes (`Utils.java`, `Main.java`).
- Changing puzzle inputs or external cache files.

### Functional Requirements
- **Part 1 Requirement:** Starting at `(row = 0, col = 0)`, traverse the grid using slope step `right 3, down 1`. Since the map repeats horizontally, horizontal indexing wraps using modulo the width of a row (`col % width`). Count and return the total number of trees (`#`) encountered until reaching beyond the bottom row.
- **Part 2 Requirement:** Evaluate tree encounters for five distinct slopes:
  1. Right 1, down 1
  2. Right 3, down 1
  3. Right 5, down 1
  4. Right 7, down 1
  5. Right 1, down 2
  Multiply all five tree encounter counts together using 64-bit integer arithmetic (`long`) to prevent integer overflow, and return the product.
- **README Documentation:** Fill in the empty placeholder entries under `### Day 03 – Toboggan Trajectory` in `2020/README.md`.

### Non-Functional Requirements
- **Performance:** Single-pass `O(H)` traversal per slope where `H` is the number of rows.
- **Precision:** 64-bit integer (`long`) product accumulation in Part 2 to avoid arithmetic overflow.

# Technical Design

### Current Implementation
- `2020/README.md` has a stub for `Day 03 – Toboggan Trajectory` with empty `- **Requirement:**` and `- **Technique:**` fields.
- `2020/src/aoc2020/Day03.java` currently contains placeholder stubs returning `input.length()` and incorrectly invokes `Utils.getInputData(2)` instead of `Utils.getInputData(3)`.

### Key Decisions
- **Grid Representation:** Represent the map as a `List<String>` or `String[]` of rows. This avoids allocating extra 2D char arrays since individual characters can be accessed directly via `charAt(col % width)`.
- **Slope Stepping Strategy:** Use a single reusable method `countTrees(List<String> grid, int right, int down)` that tracks `row` and `col` coordinates:
  - In each step: `row += down`, `col = (col + right) % width`.
  - Condition: stop when `row >= grid.size()`.
  - Check: increment tree counter if `grid.get(row).charAt(col) == '#'`.
- **Part 2 Multi-Slope Evaluation:** Use a 2D slope array `int[][] SLOPES = {{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}}` and reduce the stream / loop into a `long` product.

### Proposed Changes

#### 1. Documentation (`2020/README.md`)
Update the Day 03 section with:
```markdown
### Day 03 – Toboggan Trajectory
#### Part 1
- **Requirement:** Count trees encountered starting at top-left and following a slope of right 3, down 1 across a horizontally repeating map until reaching the bottom.
- **Technique:** 2D grid traversal using modular arithmetic for horizontal pattern repetition (`col % width`).

#### Part 2
- **Requirement:** Count trees encountered across five different slopes and find the product of all counts.
- **Technique:** Generalized slope traversal helper parameterized by `(right, down)` and 64-bit product reduction.
```

#### 2. Solution Implementation (`2020/src/aoc2020/Day03.java`)
- Define slope record or parameters: `countTrees(List<String> grid, int right, int down)`.
- Parse input with `Arrays.stream(input.strip().split("\\R")).toList()`.
- Part 1: compute `countTrees(grid, 3, 1)`.
- Part 2: compute `Arrays.stream(SLOPES).mapToLong(s -> countTrees(grid, s[0], s[1])).reduce(1L, (a, b) -> a * b)`.
- Fix `Utils.getInputData(2)` to `Utils.getInputData(3)`.

### File Structure
- `2020/README.md`: Modified (Day 3 documentation).
- `2020/src/aoc2020/Day03.java`: Modified (Day 3 solution logic & input fix).

# Testing

### Validation Approach
- **Unit / Sample Validation:** Test the slope traversal logic against the 11-row example provided in the problem description to verify:
  - Part 1 example result equals `7`.
  - Part 2 individual slopes produce counts `2`, `7`, `3`, `4`, `2` with product `336`.
- **Edge Cases Checked:**
  - `down > 1`: Stepping down 2 rows (`slope (1, 2)`) skips odd-numbered rows correctly without out-of-bounds errors on uneven row counts.
  - Horizontal wrapping when `col >= width` correctly wraps around multiple times using `% width`.
  - Integer overflow prevention in Part 2 by calculating product using `long`.

# Delivery Steps

### ✓ Step 1: Update 2020 README with Day 03 Requirements and Techniques
The 2020 README documentation includes accurate requirements and algorithmic technique descriptions for Day 3.

- Update `2020/README.md` under `### Day 03 – Toboggan Trajectory`.
- Add the Part 1 requirement: count the trees (`#`) encountered traversing the repeating grid with slope `right 3, down 1`.
- Add the Part 1 technique: 2D grid navigation with horizontal wrapping via modulo arithmetic (`(col + right) % width`).
- Add the Part 2 requirement: calculate tree counts across multiple slopes `(1,1)`, `(3,1)`, `(5,1)`, `(7,1)`, and `(1,2)`, and compute their product.
- Add the Part 2 technique: parameterized slope traversal with product accumulation using 64-bit integers (`long`).

### ✓ Step 2: Implement Grid Parsing and Slope Traversal in Day03
`Day03.java` accurately parses the input grid and counts tree collisions for any rational slope `(right, down)`.

- Implement `parseGrid(String input)` in `2020/src/aoc2020/Day03.java` to split input into rows and validate grid dimensions.
- Implement helper method `countTrees(List<String> grid, int right, int down)` using a loop incrementing `row += down` and `col = (col + right) % width` until `row >= grid.size()`.
- Implement `solvePartOne` to call `countTrees(grid, 3, 1)` and return the tree count.

### ✓ Step 3: Implement Part 2 Multi-Slope Product and Wire Day 3 Input
`Day03.java` resolves Part 2 across all slopes, computes the product safely, and correctly reads Day 3 input.

- Fix `Utils.getInputData(2)` to `Utils.getInputData(3)` in `Day03.run()`.
- Implement `solvePartTwo` to evaluate slopes `(1,1)`, `(3,1)`, `(5,1)`, `(7,1)`, and `(1,2)` against the grid.
- Multiply individual slope tree counts using `long` to avoid 32-bit integer overflow.
- Verify output formatting and logging via `LOGGER.info` in `Day03.run()`.