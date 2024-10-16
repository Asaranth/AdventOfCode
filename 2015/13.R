source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(13)
happinessChanges <- list()
people <- NULL

for (line in data) {
  parts <- strsplit(line, ' ')[[1]]
  person1 <- parts[1]
  action <- parts[3]
  value <- as.numeric(parts[4])
  person2 <- sub('\\.$', '', parts[11])

  if (action == 'lose') {
    value <- -value
  }

  if (!person1 %in% people) {
    people <- c(people, person1)
  }

  happinessChanges[[paste(person1, person2, sep = '-')]] <- value
}

calculateHappiness <- function(arrangement) {
  totalHappiness <- 0
  n <- length(arrangement)

  for (i in 1:n) {
    person1 <- arrangement[i]
    person2 <- arrangement[(i %% n) + 1]

    value1 <- ifelse(!is.null(happinessChanges[[paste(person1, person2, sep = '-')]]), happinessChanges[[paste(person1, person2, sep = '-')]], 0)
    value2 <- ifelse(!is.null(happinessChanges[[paste(person2, person1, sep = '-')]]), happinessChanges[[paste(person2, person1, sep = '-')]], 0)

    totalHappiness <- totalHappiness + value1 + value2
  }

  return(totalHappiness)
}

findMaxHappiness <- function(peopleList) {
  arrangements <- combinat::permn(peopleList)
  maxHappiness <- -Inf

  for (arrangement in arrangements) {
    totalHappiness <- calculateHappiness(arrangement)
    if (totalHappiness > maxHappiness) {
      maxHappiness <- totalHappiness
    }
  }

  return(maxHappiness)
}

solvePartOne <- function() {
  return(findMaxHappiness(people))
}

solvePartTwo <- function() {
  peopleWithSelf <- c(people, 'Me')

  for (person in people) {
    happinessChanges[[paste('Me', person, sep = '-')]] <- 0
    happinessChanges[[paste(person, 'Me', sep = '-')]] <- 0
  }

  return(findMaxHappiness(peopleWithSelf))
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')