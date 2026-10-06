#' Day 05: Doesn't He Have Intern-Elves For This?
#'
#' Validates nice versus naughty strings against vowel count, repeat character, and substring pattern rules.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(5)

#' Solves Part One: counts strings satisfying the original criteria (3+ vowels, consecutive duplicate, no forbidden pairs).
#'
#' @return Number of nice strings under Part One rules.
solvePartOne <- function() {
  findRepeat <- function(x) return(any(rle(x)$lengths > 1))

  conditionOne <- stringr::str_count(data, '[aeiou]') >= 3
  conditionTwo <- vapply(strsplit(data, ''), findRepeat, logical(1))
  conditionThree <- !(grepl('ab', data)) & !(grepl('cd', data)) & !(grepl('pq', data)) & !(grepl('xy', data))

  return(sum(conditionOne & conditionTwo & conditionThree))
}

#' Solves Part Two: counts strings satisfying updated criteria (non-overlapping pair repeat and sandwich letter repeat).
#'
#' @return Number of nice strings under Part Two rules.
solvePartTwo <- function() {
  containsPair <- function(x) stringr::str_detect(x, "([a-z][a-z]).*\\1")
  containsRepeat <- function(x) stringr::str_detect(x, "([a-z])[a-z]\\1")
  isNiceString <- function(x) containsPair(x) && containsRepeat(x)

  return(sum(sapply(data, isNiceString)))
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')