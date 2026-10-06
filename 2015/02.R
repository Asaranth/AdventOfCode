#' Day 02: I Was Told There Would Be No Math
#'
#' Calculates wrapping paper surface areas with slack and ribbon perimeters for gift boxes.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(2, 'text')

#' Solves Part One: calculates total square feet of wrapping paper needed including slack.
#'
#' @param data Character vector of box dimension strings (l x w x h).
#' @return Total wrapping paper required in square feet.
solvePartOne <- function(data) {
  total <- 0
  for (line in data) {
    dimensions <- as.numeric(strsplit(line, 'x')[[1]])
    l <- dimensions[1]
    w <- dimensions[2]
    h <- dimensions[3]
    areaSides <- c(l * w, w * h, h * l)
    total <- total + 2 * sum(areaSides) + min(areaSides)
  }
  return(total)
}

#' Solves Part Two: calculates total feet of ribbon required for smallest perimeter wrap and volume bow.
#'
#' @param data Character vector of box dimension strings (l x w x h).
#' @return Total ribbon required in feet.
solvePartTwo <- function(data) {
  total <- 0
  for (line in data) {
    dimensions <- as.numeric(strsplit(line, 'x')[[1]])
    smallestSides <- sort(dimensions)[1:2]
    total <- total + (smallestSides[1] * 2 + smallestSides[2] * 2) + prod(dimensions)
  }
  return(total)
}

cat('Part One:', solvePartOne(data), '\n')
cat('Part Two:', solvePartTwo(data), '\n')