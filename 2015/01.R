library(stringr)

con <- file('./2015/data/01.txt', open='r')
data <- readLines(con)
close(con)

solvePartOne <- function() {
  ups <- str_count(data, '\\(')
  downs <- str_count(data, '\\)')
  return(ups - downs)
}

solvePartTwo <- function() {
  floor <- 0
  index <- 0
  for (char in strsplit(data, '')[[1]]) {
    index <- index + 1
    floor <- floor + (ifelse(char == '(', 1, -1))

    if (floor == -1) {
      break
    }
  }
  return(index)
}

str_interp('Part One: ${solvePartOne()}')
str_interp('Part Two: ${solvePartTwo()}')