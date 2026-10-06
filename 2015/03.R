#' Day 03: Perfectly Spherical Houses in a Vacuum
#'
#' Tracks unique 2D coordinates visited during single and dual alternating grid deliveries.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(3)

#' Updates a 2D coordinate based on a directional movement character.
#'
#' @param pos Current 2D integer vector c(x, y).
#' @param direction Cardinal movement character ('^', 'v', '>', '<').
#' @return Updated 2D integer coordinate vector.
movePos <- function(pos, direction) {
  switch(direction,
    '>' = pos[1] <- pos[1] + 1,
    '<' = pos[1] <- pos[1] - 1,
    '^' = pos[2] <- pos[2] + 1,
    'v' = pos[2] <- pos[2] - 1
  )
  return(pos)
}

#' Checks whether a position has been visited and appends it to the visited list if novel.
#'
#' @param pos Current 2D coordinate vector c(x, y).
#' @param visited List of previously visited 2D coordinate vectors.
#' @return A list containing the increment count (1 if new, 0 if already visited) and the updated visited list.
checkAndMarkVisited <- function(pos, visited) {
  total <- 0
  if (!Position(function(x) identical(x, pos), visited, nomatch=0) > 0) {
    visited[[1 + length(visited)]] <- pos
    total <- 1
  }
  return(list(total, visited))
}

#' Solves Part One: counts the total number of unique houses visited by Santa alone.
#'
#' @return Total unique houses visited.
solvePartOne <- function() {
  total <- 1
  pos <- c(0, 0)
  visited <- list(pos)

  for (direction in strsplit(data, '')[[1]]) {
    pos <- movePos(pos, direction)
    result <- checkAndMarkVisited(pos, visited)
    total <- total + result[[1]]
    visited <- result[[2]]
  }
  return(total)
}

#' Solves Part Two: counts total unique houses visited by Santa and Robo-Santa moving in alternation.
#'
#' @return Total unique houses visited by both agents.
solvePartTwo <- function() {
  total <- 1
  positions <- list(santa = c(0, 0), robot = c(0, 0))
  current <- "santa"
  visited <- list(positions$santa)

  for (direction in strsplit(data, '')[[1]]) {
    positions[[current]] <- movePos(positions[[current]], direction)
    result <- checkAndMarkVisited(positions[[current]], visited)
    total <- total + result[[1]]
    visited <- result[[2]]
    current <- ifelse(current == "santa", "robot", "santa")
  }
  return(total)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')