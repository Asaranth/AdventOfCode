library(stringr)
library(tokenizers)

con <- file('./2015/data/05.txt', open='r')
data <- readLines(con)
close(con)

solvePartOne <- function() {
  findRepeat <- function(x) {
    any(rle(x)$lengths > 1)
  }

  conditionOne <- str_count(data, '[aeiou]') >= 3
  conditionTwo <- vapply(strsplit(data, ''), findRepeat, logical(1))
  conditionThree <- !(grepl('ab', data)) & !(grepl('cd', data)) & !(grepl('pq', data)) & !(grepl('xy', data))

  return(sum(conditionOne & conditionTwo & conditionThree))
}

solvePartTwo <- function() {
  pairRegex <- "([a-z][a-z]).*\\1"
  repeatRegex <- "([a-z])[a-z]\\1"

  containsPair <- function(x) str_detect(x, pairRegex)
  containsRepeat <- function(x) str_detect(x, repeatRegex)

  isNiceString <- function(x) containsPair(x) && containsRepeat(x)

  return(sum(sapply(data, isNiceString)))
}

str_interp('Part One: ${solvePartOne()}')
str_interp('Part Two: ${solvePartTwo()}')