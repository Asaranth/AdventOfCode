data <- read.csv('./2015/data/02.txt', header=FALSE)

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
    smallestSides <- sort(dimensions)[1:2]
    total <- total + (smallestSides[1] * 2 + smallestSides[2] * 2) + prod(dimensions)
  }
  return(total)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')