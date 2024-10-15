data <- readLines('./2015/data/09.txt')

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

solvePartOne <- function(routeDistances) {
  return(min(routeDistances))
}

solvePartTwo <- function(routeDistances) {
  return(max(routeDistances))
}

parsedData <- parseDistances(data)
routeDistances <- calculateAllRouteDistances(parsedData)

cat('Part One:', solvePartOne(routeDistances), '\n')
cat('Part Two:', solvePartTwo(routeDistances), '\n')