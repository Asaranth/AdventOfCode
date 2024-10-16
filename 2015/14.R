source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(14)
raceTime <- 2503

parseReindeer <- function(line) {
  parts <- strsplit(line, ' ')[[1]]
  name <- parts[1]
  speed <- as.numeric(parts[4])
  flyTime <- as.numeric(parts[7])
  restTime <- as.numeric(parts[14])
  return(list(name = name, speed = speed, flyTime = flyTime, restTime = restTime))
}

reindeers <- lapply(data, parseReindeer)

calculateDistance <- function(reindeer, raceTime) {
  cycleTime <- reindeer$flyTime + reindeer$restTime
  fullCycles <- raceTime %/% cycleTime
  remainingTime <- raceTime %% cycleTime
  effectiveFlyTime <- min(remainingTime, reindeer$flyTime)
  totalDistance <- (fullCycles * reindeer$flyTime + effectiveFlyTime) * reindeer$speed
  return(totalDistance)
}

solvePartOne <- function() {
  distances <- vapply(reindeers, function(r) calculateDistance(r, raceTime), numeric(1))
  maxDistance <- max(distances)
  return(maxDistance)
}

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