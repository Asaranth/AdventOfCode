source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(12)
json <- jsonlite::fromJSON(paste(data, collapse = ''))

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

solvePartOne <- function() {
  total <- sumNumbers(json, ignoreRed = FALSE)
  return(total)
}

solvePartTwo <- function() {
  total <- sumNumbers(json, ignoreRed = TRUE)
  return(total)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')