#' Day 01: Not Quite Lisp
#'
#' Simulates floor traversal via parenthesis counting and first-passage prefix sum tracking.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(1)

#' Solves Part One: computes final floor by subtracting down steps from up steps.
#'
#' @return The final floor number reached.
solvePartOne <- function() {
  ups <- stringr::str_count(data, '\\(')
  downs <- stringr::str_count(data, '\\)')
  return(ups - downs)
}

#' Solves Part Two: finds the 1-based index of the first character causing entry into the basement (floor -1).
#'
#' @return The 1-based index where the floor first becomes -1.
solvePartTwo <- function() {
  inputData <- strsplit(data, '')[[1]]
  indexVector <- seq_along(inputData)
  floorVector <- cumsum(ifelse(inputData == '(', 1, -1))
  return(min(indexVector[floorVector == -1]))
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')