#' Day 09: All in a Single Night
#'
#' Solves travelling salesperson route lengths across all location permutations.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(9)

#' Parses pairwise city distance strings into a lookup dictionary and unique location list.
#'
#' @param data Character vector of distance descriptions.
#' @return A list containing pairwise distance list and vector of unique location names.
parseDistances <- function(data) {
  distances <- list()
  locations <- NULL

  for (line in data) {
    matches <- stringr::str_match(line, '(\\w+) to (\\w+) = (\\d+)')
    loc1 <- matches[2]
    loc2 <- matches[3]
    dist <- as.numeric(matches[4])

    distances[[paste(sort(c(loc1, loc2)), collapse = '-')]] <- dist
    locations <- unique(c(locations, loc1, loc2))
  }
  return(list(distances = distances, locations = locations))
}

#' Computes total route distance for all Hamiltonian path permutations visiting every location.
#'
#' @param parsedData List containing distances lookup and unique location names.
#' @return Numeric vector of total route distances for all permutations.
calculateAllRouteDistances <- function(parsedData) {
  permutations <- combinat::permn(parsedData$locations)
  routeDistances <- numeric(length(permutations))
  for (i in seq_along(permutations)) {
    route <- permutations[[i]]
    totalDistance <- 0
    for (j in 1:(length(route) - 1)) {
      locPair <- paste(sort(c(route[j], route[j + 1])), collapse = '-')
      totalDistance <- totalDistance + parsedData$distances[[locPair]]
    }
    routeDistances[i] <- totalDistance
  }
  return(routeDistances)
}

#' Solves Part One: finds the minimum route distance visiting all locations.
#'
#' @param routeDistances Numeric vector of all route distances.
#' @return Minimum route distance.
solvePartOne <- function(routeDistances) {
  return(min(routeDistances))
}

#' Solves Part Two: finds the maximum route distance visiting all locations.
#'
#' @param routeDistances Numeric vector of all route distances.
#' @return Maximum route distance.
solvePartTwo <- function(routeDistances) {
  return(max(routeDistances))
}

parsedData <- parseDistances(data)
routeDistances <- calculateAllRouteDistances(parsedData)

cat('Part One:', solvePartOne(routeDistances), '\n')
cat('Part Two:', solvePartTwo(routeDistances), '\n')