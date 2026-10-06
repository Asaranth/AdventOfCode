#' Day 14: Reindeer Olympics
#'
#' Simulates reindeer flight-and-rest cycles to evaluate total distance and lead point standings.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(14)
raceTime <- 2503

#' Parses a reindeer flight specification line into attributes.
#'
#' @param line Input line describing flight speed, fly time, and rest duration.
#' @return A list containing reindeer name, speed, flyTime, and restTime.
parseReindeer <- function(line) {
  parts <- strsplit(line, ' ')[[1]]
  name <- parts[1]
  speed <- as.numeric(parts[4])
  flyTime <- as.numeric(parts[7])
  restTime <- as.numeric(parts[14])
  return(list(name = name, speed = speed, flyTime = flyTime, restTime = restTime))
}

reindeers <- lapply(data, parseReindeer)

#' Calculates the distance travelled by a reindeer after a specified elapsed time.
#'
#' @param reindeer Reindeer specification list with speed, flyTime, and restTime.
#' @param raceTime Elapsed time in seconds.
#' @return Total distance travelled in kilometres.
calculateDistance <- function(reindeer, raceTime) {
  cycleTime <- reindeer$flyTime + reindeer$restTime
  fullCycles <- raceTime %/% cycleTime
  remainingTime <- raceTime %% cycleTime
  effectiveFlyTime <- min(remainingTime, reindeer$flyTime)
  totalDistance <- (fullCycles * reindeer$flyTime + effectiveFlyTime) * reindeer$speed
  return(totalDistance)
}

#' Solves Part One: computes the maximum distance travelled by any reindeer after the race time.
#'
#' @return Maximum distance in kilometres.
solvePartOne <- function() {
  distances <- vapply(reindeers, function(r) calculateDistance(r, raceTime), numeric(1))
  maxDistance <- max(distances)
  return(maxDistance)
}

#' Solves Part Two: calculates points awarded at each second to leaders and finds highest score.
#'
#' @return Maximum points scored by any reindeer.
solvePartTwo <- function() {
  points <- setNames(rep(0, length(reindeers)), sapply(reindeers, function(r) r$name))
  for (second in 1:raceTime) {
    distances <- sapply(reindeers, function(r) calculateDistance(r, second))
    leadDistance <- max(distances)
    leaders <- which(distances == leadDistance)
    for (leader in leaders) {
      points[leader] <- points[leader] + 1
    }
  }
  maxPoints <- max(points)
  return(maxPoints)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')