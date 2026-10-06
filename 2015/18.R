#' Day 18: Like a GIF For Your Yard
#'
#' Simulates a 2D cellular automaton (Conway's Game of Life) with optional stuck corner lights.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(18)

#' Builds the initial 100x100 character matrix grid from input strings.
#'
#' @return 100x100 character matrix of '#' (on) and '.' (off).
buildGrid <- function() {
  initialData <- data
  grid <- matrix(unlist(strsplit(initialData, '')), nrow = 100, byrow = TRUE)
  return(grid)
}

#' Counts the number of active ('#') neighbours surrounding a cell in the 8 cardinal and intercardinal directions.
#'
#' @param grid Character matrix representing light grid state.
#' @param row 1-based row index.
#' @param col 1-based column index.
#' @return Number of neighbouring active lights (0 to 8).
countOnNeighbors <- function(grid, row, col) {
  directions <- list(c(-1, -1), c(-1, 0), c(-1, 1), c(0, -1), c(0, 1), c(1, -1), c(1, 0), c(1, 1))
  count <- 0
  for (dir in directions) {
    newRow <- row + dir[1]
    newCol <- col + dir[2]
    if (newRow >= 1 && newRow <= nrow(grid) && newCol >= 1 && newCol <= ncol(grid)) {
      if (grid[newRow, newCol] == '#') {
        count <- count + 1
      }
    }
  }
  return(count)
}

#' Forces the four corner lights of the grid to be on ('#').
#'
#' @param grid Character matrix representing light grid state.
#' @return Updated character matrix with all 4 corners set to '#'.
setCornersOn <- function(grid) {
  grid[1, 1] <- '#'
  grid[1, ncol(grid)] <- '#'
  grid[nrow(grid), 1] <- '#'
  grid[nrow(grid), ncol(grid)] <- '#'
  return(grid)
}

#' Advances the grid by one animation step according to neighbour rules.
#'
#' @param currentGrid Current grid character matrix.
#' @param keepCornersOn Logical flag indicating whether four corners must remain permanently on.
#' @return Next state character matrix.
animate <- function(currentGrid, keepCornersOn = FALSE) {
  newGrid <- currentGrid
  for (row in seq_len(nrow(currentGrid))) {
    for (col in seq_len(ncol(currentGrid))) {
      onNeighbors <- countOnNeighbors(currentGrid, row, col)
      if (currentGrid[row, col] == '#') {
        newGrid[row, col] <- ifelse(onNeighbors == 2 || onNeighbors == 3, '#', '.')
      } else {
        newGrid[row, col] <- ifelse(onNeighbors == 3, '#', '.')
      }
    }
  }
  if (keepCornersOn) {
    newGrid <- setCornersOn(newGrid)
  }
  return(newGrid)
}

#' Solves Part One: steps the grid 100 times under standard rules and counts lit lights.
#'
#' @return Total lit lights after 100 steps.
solvePartOne <- function() {
  grid <- buildGrid()
  for (i in 1:100) {
    grid <- animate(grid)
  }
  return(sum(grid == '#'))
}

#' Solves Part Two: steps the grid 100 times with four corner lights permanently on and counts lit lights.
#'
#' @return Total lit lights after 100 steps with fixed corners.
solvePartTwo <- function() {
  grid <- buildGrid()
  grid <- setCornersOn(grid)
  for (i in 1:100) {
    grid <- animate(grid, TRUE)
  }
  return(sum(grid == '#'))
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')