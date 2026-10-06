#' Day 20: Infinite Elves and Infinite Houses
#'
#' Calculates factor sum present delivery totals to find lowest house numbers exceeding thresholds.

source(file.path(getwd(), '2015/utils.R'))
data <- as.integer(getInputData(20))

#' Computes total presents delivered to a house based on its divisors and elf delivery limits.
#'
#' @param house 1-based integer house index.
#' @param multiplier Multiplier for present calculation per elf.
#' @param maxDeliveries Maximum deliveries allowed per elf before stopping.
#' @return Total presents received by the house.
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

#' Solves Part One: finds lowest house receiving at least the target number of presents with unlimited deliveries.
#'
#' @return Lowest house number for Part One.
solvePartOne <- function() {
  house <- 1
  repeat {
    if (sumPresents(house) >= data) {
      return(house)
    }
    house <- house + 1
  }
}

#' Solves Part Two: finds lowest house receiving at least the target presents with 50-delivery limits and 11x multiplier.
#'
#' @return Lowest house number for Part Two.
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