source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(16)
MFCSAM <- c(
  children = 3,
  cats = 7,
  samoyeds = 2,
  pomeranians = 3,
  akitas = 0,
  vizslas = 0,
  goldfish = 5,
  trees = 3,
  cars = 2,
  perfumes = 1
)

parseSues <- function(data) {
  sues <- list()
  for (line in data) {
    splitLine <- unlist(strsplit(line, ': |, '))
    sueNo <- as.integer(gsub('Sue ', '', splitLine[1]))
    attributes <- splitLine[-1]
    sueData <- list()
    for (i in seq(1, length(attributes), 2)) {
      sueData[[attributes[i]]] <- as.integer(attributes[i + 1])
    }
    sues[[as.character(sueNo)]] <- sueData
  }
  return(sues)
}

sues <- parseSues(data)

solvePartOne <- function() {
  for (sueNo in names(sues)) {
    sueData <- sues[[sueNo]]
    match <- TRUE

    for (attribute in names(sueData)) {
      if (!is.na(MFCSAM[attribute]) && sueData[[attribute]] != MFCSAM[attribute]) {
        match <- FALSE
        break
      }
    }

    if (match) {
      return(sueNo)
    }
  }

  return(NA)
}

solvePartTwo <- function() {
  for (sueNo in names(sues)) {
    sueData <- sues[[sueNo]]
    match <- TRUE

    for (attribute in names(sueData)) {
      if (!is.na(MFCSAM[attribute])) {
        if (attribute %in% c("cats", "trees")) {
          if (sueData[[attribute]] <= MFCSAM[attribute]) {
            match <- FALSE
            break
          }
        } else if (attribute %in% c("pomeranians", "goldfish")) {
          if (sueData[[attribute]] >= MFCSAM[attribute]) {
            match <- FALSE
            break
          }
        } else {
          if (sueData[[attribute]] != MFCSAM[attribute]) {
            match <- FALSE
            break
          }
        }
      }
    }

    if (match) {
      return(sueNo)
    }
  }

  return(NA)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')