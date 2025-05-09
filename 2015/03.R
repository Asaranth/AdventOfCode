source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(3)

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