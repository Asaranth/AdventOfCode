# Advent of Code 2025 — Solution Notes

Each day is split into Part 1 and Part 2 with a short note on what technique was used and why. Titles are left as placeholders so they can be filled with the official AoC names later.

### Day 01 - [Title TBD]

#### Part 1
- Technique: circular track simulation with modular arithmetic.
- What/Why: Walk a pointer around a ring of size 100 following `L/Rk`. Count a “hit” when the pointer lands on index 0.

#### Part 2
- Technique: arithmetic wrap counting via integer-division deltas (no step-by-step simulation).
- What/Why: Compute how many times a contiguous left/right move crosses multiples of the track size using `div` differences, summing hits analytically.

### Day 02 - [Title TBD]

#### Part 1
- Technique: range parsing; string test on halves.
- What/Why: Expand numeric ranges, test each number’s decimal form for even length and exact half-duplication (first half equals second half).

#### Part 2
- Technique: periodic string detection via the classic rotation trick.
- What/Why: Detect whether a string is composed of repeated blocks by checking membership in `(s+s)` minus its first and last chars.

### Day 03 - [Title TBD]

#### Part 1
- Technique: greedy windowed selection; fold digits to an integer.
- What/Why: From each bank of digits, repeatedly pick the maximum feasible next digit to maximize a 2-digit number.

#### Part 2
- Technique: same greedy construction scaled to longer output.
- What/Why: Build the lexicographically largest possible 12-digit number using the same greedy-in-window method.

### Day 04 - [Title TBD]

#### Part 1
- Technique: grid neighbor-degree check on 8-neighborhood.
- What/Why: Count cells marked `@` that have fewer than 4 occupied neighbors; uses bounds checking over the 8 surrounding cells.

#### Part 2
- Technique: iterative pruning (peeling) process.
- What/Why: Repeatedly remove “accessible” `@` cells (degree < 4) and continue until none remain, summing all removed.

### Day 05 - [Title TBD]

#### Part 1
- Technique: interval containment using parsed ranges + set of candidates.
- What/Why: Count available IDs that fall within any “fresh” range.

#### Part 2
- Technique: interval merge and length sum.
- What/Why: Sort and merge overlapping/adjacent ranges, then sum merged lengths for the total coverage.

### Day 06 - [Title TBD]

#### Part 1
- Technique: columnar parsing with `transpose`; reduce numbers by trailing operator.
- What/Why: Treat each column as numbers plus an operator at the end; apply `+` or `*` to aggregate the numbers.

#### Part 2
- Technique: right-to-left streaming accumulation; batch by encountering an operator.
- What/Why: Scan columns from the right, collect digits into numbers until an operator column is found, then aggregate and reset.

### Day 07 - [Title TBD]

#### Part 1
- Technique: scanline-like simulation of beam splits with dedup/sort.
- What/Why: Track beam columns per row; `^` causes left/right splits. Count total splits as beams traverse rows.

#### Part 2
- Technique: dynamic programming over columns with unboxed vectors.
- What/Why: Propagate the number of beams per column row-by-row; `^` doubles to neighbors. Sum exits at the bottom row.

### Day 08 - [Title TBD]

#### Part 1
- Technique: Kruskal-like DSU over the 1000 shortest edges.
- What/Why: Form components by uniting near neighbors; compute and multiply the top three component sizes.

#### Part 2
- Technique: full edge sweep with DSU until single-component convergence.
- What/Why: Process sorted edges; when the final union occurs, output a product of the uniting points’ x-coordinates.

### Day 09 - [Title TBD]

#### Part 1
- Technique: combinatorial rectangle search over all point pairs.
- What/Why: For each pair of red points, compute the axis-aligned rectangle area and take the maximum.

#### Part 2
- Technique: AABB collision checks against a rectilinear polyline.
- What/Why: Build the polygonal chain from the red points, and only consider rectangles whose interiors do not intersect any horizontal/vertical edge. Among those, take the maximum area.
