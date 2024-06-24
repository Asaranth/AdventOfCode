library(stringr)
con <- file('./2015/data/03.txt', open='r')
data <- readLines(con)
close(con)

movePos <- function(pos, direction) {
  switch(direction,
    '>' = pos[1] <- pos[1] + 1,
    '<' = pos[1] <- pos[1] - 1,
    '^' = pos[2] <- pos[2] + 1,
    'v' = pos[2] <- pos[2] - 1
  )
  return(pos)
}

checkAndMarkVisited <- function(pos, visited) {
  total <- 0
  if (!Position(function(x) identical(x, pos), visited, nomatch=0) > 0) {
    visited[[1 + length(visited)]] <- pos
    total <- 1
  }
  return(list(total, visited))
}

solvePartOne <- function(data) {
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

solvePartTwo <- function(data) {
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

str_interp('Part One: ${solvePartOne(data)}')
str_interp('Part Two: ${solvePartTwo(data)}')