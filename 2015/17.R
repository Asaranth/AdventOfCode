source(file.path(getwd(), '2015/utils.R'))
data <- as.numeric(getInputData(17))

findValidSubsets <- function(data, totalEggnog) {
  validSubsets <- list()
  for (i in seq_along(data)) {
    subsets <- combn(data, i, simplify = FALSE)
    for (subset in subsets) {
      if (sum(subset) == totalEggnog) {
        validSubsets <- c(validSubsets, list(subset))
      }
    }
  }
  return(validSubsets)
}

solvePartOne <- function() {
  validSubsets <- findValidSubsets(data, 150)
  return(length(validSubsets))
}

solvePartTwo <- function() {
  validSubsets <- findValidSubsets(data, 150)
  if (length(validSubsets) == 0) return(0)
  minContainers <- min(sapply(validSubsets, length))
  return(sum(sapply(validSubsets, length) == minContainers))
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')