data <- readLines('./2015/data/05.txt')

solvePartOne <- function() {
  findRepeat <- function(x) return(any(rle(x)$lengths > 1))

  conditionOne <- stringr::str_count(data, '[aeiou]') >= 3
  conditionTwo <- vapply(strsplit(data, ''), findRepeat, logical(1))
  conditionThree <- !(grepl('ab', data)) & !(grepl('cd', data)) & !(grepl('pq', data)) & !(grepl('xy', data))

  return(sum(conditionOne & conditionTwo & conditionThree))
}

solvePartTwo <- function() {
  containsPair <- function(x) stringr::str_detect(x, "([a-z][a-z]).*\\1")
  containsRepeat <- function(x) stringr::str_detect(x, "([a-z])[a-z]\\1")
  isNiceString <- function(x) containsPair(x) && containsRepeat(x)

  return(sum(sapply(data, isNiceString)))
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')