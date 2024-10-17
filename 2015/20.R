source(file.path(getwd(), '2015/utils.R'))
data <- as.integer(getInputData(20))

sumPresents <- function(house, multiplier = 10, maxDeliveries = Inf) {
  totalPresents <- 0
  for (elf in 1:floor(sqrt(house))) {
    if (house %% elf == 0) {
      if (house / elf <= maxDeliveries) {
        totalPresents <- totalPresents + elf * multiplier
      }
      if (elf != house / elf && elf <= maxDeliveries) {
        totalPresents <- totalPresents + (house / elf) * multiplier
      }
    }
  }
  return(totalPresents)
}

solvePartOne <- function() {
  house <- 1
  repeat {
    if (sumPresents(house) >= data) {
      return(house)
    }
    house <- house + 1
  }
}

solvePartTwo <- function() {
  house <- 1
  repeat {
    if (sumPresents(house, 11, 50) >= data) {
      return(house)
    }
    house <- house + 1
  }
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')