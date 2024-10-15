data <- readLines('./2015/data/12.txt')
json <- jsonlite::fromJSON(paste(data, collapse = ''))

sum_numbers <- function(x, ignore_red = FALSE) {
  if (is.numeric(x)) {
    return(x)
  } else if (is.list(x)) {
    if (ignore_red && !is.null(names(x)) && any(sapply(x, identical, "red"))) {
      return(0)
    }
    sum_elements <- sapply(x, sum_numbers, ignore_red = ignore_red)
    return(sum(unlist(sum_elements), na.rm = TRUE))
  } else if (is.character(x)) {
    nums <- as.numeric(unlist(regmatches(x, gregexpr('-?\\d+\\.?\\d*', x))))
    return(sum(nums, na.rm = TRUE))
  } else {
    return(0)
  }
}

solvePartOne <- function() {
  sum_total <- sum_numbers(json, ignore_red = FALSE)
  return(sum_total)
}

solvePartTwo <- function() {
  sum_total <- sum_numbers(json, ignore_red = TRUE)
  return(sum_total)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')