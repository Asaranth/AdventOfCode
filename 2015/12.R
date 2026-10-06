#' Day 12: JSAbacusFramework.io
#'
#' Recursively parses and sums numbers within JSON structures with conditional object filtering.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(12)
json <- jsonlite::fromJSON(paste(data, collapse = ''))

#' Recursively sums all numbers within a JSON data element, optionally ignoring objects with a "red" property.
#'
#' @param x Parsed JSON object, list, vector, or primitive.
#' @param ignoreRed Logical flag indicating whether to ignore named objects with "red" property values.
#' @return Numeric sum of all contained numbers.
sumNumbers <- function(x, ignoreRed = FALSE) {
  if (is.numeric(x)) {
    return(x)
  } else if (is.list(x)) {
    if (ignoreRed && !is.null(names(x)) && any(sapply(x, identical, "red"))) {
      return(0)
    }
    sumElements <- sapply(x, sumNumbers, ignoreRed = ignoreRed)
    return(sum(unlist(sumElements), na.rm = TRUE))
  } else if (is.character(x)) {
    nums <- as.numeric(unlist(regmatches(x, gregexpr('-?\\d+\\.?\\d*', x))))
    return(sum(nums, na.rm = TRUE))
  } else {
    return(0)
  }
}

#' Solves Part One: computes the sum of all numbers throughout the JSON document.
#'
#' @return Sum of all numbers.
solvePartOne <- function() {
  total <- sumNumbers(json, ignoreRed = FALSE)
  return(total)
}

#' Solves Part Two: computes the sum of all numbers while excluding objects containing "red" property values.
#'
#' @return Filtered sum of all numbers.
solvePartTwo <- function() {
  total <- sumNumbers(json, ignoreRed = TRUE)
  return(total)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')