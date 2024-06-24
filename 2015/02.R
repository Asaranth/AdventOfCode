library(stringr)

con <- file('./2015/data/02.txt', open='r')
data <- read.csv(con, header=FALSE)
close(con)

solvePartOne <- function() {
  total <- 0
  for (present in data[[1]]) {
    dimensions <- as.numeric(strsplit(present, 'x')[[1]])
    l <- dimensions[1]
    w <- dimensions[2]
    h <- dimensions[3]
    areaSides <- c(l * w, w * h, h * l)
    total <- total + 2 * sum(areaSides) + min(areaSides)
  }
  return(total)
}

solvePartTwo <- function() {
  total <- 0
  for (present in data[[1]]) {
    dimensions <- as.numeric(strsplit(present, 'x')[[1]])
    l <- dimensions[1]
    w <- dimensions[2]
    h <- dimensions[3]
    smallestSides <- sort(dimensions)[1:2]
    total <- total + (smallestSides[1] * 2 + smallestSides[2] * 2) + (l * w * h)
  }
  return(total)
}

str_interp('Part One: ${solvePartOne()}')
str_interp('Part Two: ${solvePartTwo()}')