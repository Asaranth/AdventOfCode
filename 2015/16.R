#' Day 16: Aunt Sue
#'
#' Filters Aunt Sue candidates using exact and range-adjusted MFCSAM retroencabulator readings.

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

#' Parses raw Aunt Sue gift memory descriptions into structured attribute lists.
#'
#' @param data Character vector of Sue descriptions.
#' @return Named list mapping Sue IDs to lists of known attribute values.
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

#' Solves Part One: finds Sue ID whose recorded attributes exactly match MFCSAM readings.
#'
#' @return ID of the matching Aunt Sue.
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

#' Solves Part Two: finds Sue ID matching retroencabulator ranged criteria (cats/trees greater, pomeranians/goldfish fewer).
#'
#' @return ID of the matching Aunt Sue under ranged rules.
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