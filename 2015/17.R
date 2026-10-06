#' Day 17: No Such Thing as Too Much
#'
#' Finds container combinations and minimum subset counts summing to the target eggnog volume.

source(file.path(getwd(), '2015/utils.R'))
data <- as.numeric(getInputData(17))

#' Finds all subset combinations of containers whose capacities sum exactly to totalEggnog.
#'
#' @param data Numeric vector of container capacities.
#' @param totalEggnog Target volume in litres.
#' @return List of container capacity numeric vectors that sum to the target.
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

#' Solves Part One: counts the total number of combinations of containers that can hold 150 litres.
#'
#' @return Total valid container combinations.
solvePartOne <- function() {
  validSubsets <- findValidSubsets(data, 150)
  return(length(validSubsets))
}

#' Solves Part Two: counts combinations that use the minimum possible number of containers.
#'
#' @return Number of valid combinations with minimal container count.
solvePartTwo <- function() {
  validSubsets <- findValidSubsets(data, 150)
  if (length(validSubsets) == 0) return(0)
  minContainers <- min(sapply(validSubsets, length))
  return(sum(sapply(validSubsets, length) == minContainers))
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')