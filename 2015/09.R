data <- readLines('./2015/data/09.txt')

parse_distances <- function(data) {
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

calculate_all_route_distances <- function(parsed_data) {
  all_permutations <- combinat::permn(parsed_data$locations)
  route_distances <- numeric(length(all_permutations))
  for (i in seq_along(all_permutations)) {
    route <- all_permutations[[i]]
    total_distance <- 0
    for (j in 1:(length(route) - 1)) {
      loc_pair <- paste(sort(c(route[j], route[j + 1])), collapse = '-')
      total_distance <- total_distance + parsed_data$distances[[loc_pair]]
    }
    route_distances[i] <- total_distance
  }
  return(route_distances)
}

solvePartOne <- function(route_distances) {
  return(min(route_distances))
}

solvePartTwo <- function(route_distances) {
  return(max(route_distances))
}

parsed_data <- parse_distances(data)
route_distances <- calculate_all_route_distances(parsed_data)

cat(stringr::str_interp('Part One: ${solvePartOne(route_distances)}\n'))
cat(stringr::str_interp('Part Two: ${solvePartTwo(route_distances)}\n'))