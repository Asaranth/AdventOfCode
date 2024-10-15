library(magrittr)

data <- readLines('./2015/data/06.txt')

turnOn <- function(grid, x1, y1, x2, y2) {
  grid <- grid %>% plyr::mutate(lit = ifelse((x >= x1 & x <= x2) & (y >= y1 & y <= y2), TRUE, lit))
  return(grid)
}

turnOff <- function(grid, x1, y1, x2, y2) {
  grid <- grid %>% plyr::mutate(lit = ifelse((x >= x1 & x <= x2) & (y >= y1 & y <= y2), FALSE, lit))
  return(grid)
}

toggle <- function(grid, x1, y1, x2, y2) {
  grid <- grid %>% plyr::mutate(lit = ifelse((x >= x1 & x <= x2) & (y >= y1 & y <= y2), !lit, lit))
  return(grid)
}

adjustBrightness <- function(grid, x1, y1, x2, y2, change) {
  grid <- grid %>% plyr::mutate(brightness = ifelse((x >= x1 & x <= x2) & (y >= y1 & y <= y2), pmax(brightness + change, 0), brightness))
  return(grid)
}

solvePartOne <- function() {
  grid <- expand.grid(x = seq(from=0, by=1, l=1000), y = seq(from=0, by=1, l=1000), lit = FALSE)

  for (instruction in data) {
    components <- strsplit(instruction, ' ')[[1]]

    if (components[1] == 'toggle') {
      operation <- components[1]
      startData <- components[2]
      endData <- components[4]
    } else {
      operation <- paste(components[1], components[2])
      startData <- components[3]
      endData <- components[5]
    }

    start_coords <- as.integer(unlist(strsplit(startData, ',')))
    end_coords <- as.integer(unlist(strsplit(endData, ',')))

    if (operation == 'turn on') {
      grid <- turnOn(grid, start_coords[1], start_coords[2], end_coords[1], end_coords[2])
    } else if (operation == 'turn off') {
      grid <- turnOff(grid, start_coords[1], start_coords[2], end_coords[1], end_coords[2])
    } else if (operation == 'toggle') {
      grid <- toggle(grid, start_coords[1], start_coords[2], end_coords[1], end_coords[2])
    }
  }

  return(sum(grid$lit == TRUE))
}

solvePartTwo <- function() {
  grid <- expand.grid(x = seq(from=0, by=1, l=1000), y = seq(from=0, by=1, l=1000), brightness = 0)

  for (instruction in data) {
    components <- strsplit(instruction, ' ')[[1]]

    if (components[1] == 'toggle') {
      operation <- components[1]
      startData <- components[2]
      endData <- components[4]
    } else {
      operation <- paste(components[1], components[2])
      startData <- components[3]
      endData <- components[5]
    }

    start_coords <- as.integer(unlist(strsplit(startData, ',')))
    end_coords <- as.integer(unlist(strsplit(endData, ',')))

    if (operation == 'turn on') {
      grid <- adjustBrightness(grid, start_coords[1], start_coords[2], end_coords[1], end_coords[2], 1)
    } else if (operation == 'turn off') {
      grid <- adjustBrightness(grid, start_coords[1], start_coords[2], end_coords[1], end_coords[2], -1)
    } else if (operation == 'toggle') {
      grid <- adjustBrightness(grid, start_coords[1], start_coords[2], end_coords[1], end_coords[2], 2)
    }
  }

  return(sum(grid$brightness))
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')