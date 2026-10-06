#' Day 24: It Hangs in the Balance
#'
#' Partitions packages into balanced weight groups to minimise passenger compartment size and quantum entanglement.

source(file.path(getwd(), '2015/utils.R'))
data <- as.numeric(getInputData(24))

#' Finds package subsets of minimal size that sum to the target group weight.
#'
#' @param data Numeric vector of package weights.
#' @param target Target sum weight for each partition.
#' @return Matrix where each column is a valid minimal subset, or NULL if none.
findCombinations <- function(data, target) {
  for (i in seq_along(data)) {
    combs <- combn(data, i)
    colSumsCombs <- colSums(combs)
    if (any(colSumsCombs == target)) {
      return(combs[, which(colSumsCombs == target)])
    }
  }
  return(NULL)
}

#' Finds the optimal quantum entanglement for the passenger compartment across equal partition groups.
#'
#' @param data Numeric vector of package weights.
#' @param groups Number of partition groups (3 for Part 1, 4 for Part 2).
#' @return Minimum quantum entanglement (product of weights) of the optimal group.
findOptimalGroup <- function(data, groups) {
  target <- sum(data) / groups
  firstGroup <- findCombinations(data, target)

  if (is.null(firstGroup)) {
    stop('No valid group found')
  }

  remainingData <- setdiff(data, firstGroup[, 1])
  findCombinations(remainingData, target)

  qe <- apply(firstGroup, 2, prod)
  index <- order(qe)[1]

  return(qe[index])
}

#' Solves Part One: finds optimal quantum entanglement when dividing packages into 3 groups.
#'
#' @return Minimum quantum entanglement for 3 groups.
solvePartOne <- function() {
  return(findOptimalGroup(data, 3))
}

#' Solves Part Two: finds optimal quantum entanglement when dividing packages into 4 groups.
#'
#' @return Minimum quantum entanglement for 4 groups.
solvePartTwo <- function() {
  return(findOptimalGroup(data, 4))
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')