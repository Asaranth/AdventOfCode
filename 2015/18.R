source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(18)

buildGrid <- function() {
  initialData <- data
  grid <- matrix(unlist(strsplit(initialData, '')), nrow = 100, byrow = TRUE)
  return(grid)
}

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

setCornersOn <- function(grid) {
  grid[1, 1] <- '#'
  grid[1, ncol(grid)] <- '#'
  grid[nrow(grid), 1] <- '#'
  grid[nrow(grid), ncol(grid)] <- '#'
  return(grid)
}

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

solvePartOne <- function() {
  grid <- buildGrid()
  for (i in 1:100) {
    grid <- animate(grid)
  }
  return(sum(grid == '#'))
}

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