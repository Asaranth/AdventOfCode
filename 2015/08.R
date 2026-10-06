#' Day 08: Matchsticks
#'
#' Computes string memory versus representation length differences under character escaping.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(8)

#' Calculates the number of characters in the raw code representation of a string literal.
#'
#' @param s Raw string literal.
#' @return Number of raw characters in code.
calculateCodeLength <- function(s) {
  return(nchar(s))
}

#' Calculates the number of characters in the evaluated in-memory string value.
#'
#' @param s Raw string literal.
#' @return Number of characters in memory.
calculateMemoryLength <- function(s) {
  s <- substring(s, 2, nchar(s) - 1)
  i <- 1
  inMemoryLength <- 0
  while (i <= nchar(s)) {
    char <- substring(s, i, i)
    if (char == '\\') {
      nextChar <- substring(s, i+1, i+1)
      if (nextChar %in% c('\\', '\"')) {
        inMemoryLength <- inMemoryLength + 1
        i <- i + 2
      } else if (nextChar == 'x') {
        inMemoryLength <- inMemoryLength + 1
        i <- i + 4
      } else {
        inMemoryLength <- inMemoryLength + 1
        i <- i + 1
      }
    } else {
      inMemoryLength <- inMemoryLength + 1
      i <- i + 1
    }
  }
  return(inMemoryLength)
}

#' Calculates the number of characters needed to encode a string literal with additional escaping.
#'
#' @param s Raw string literal.
#' @return Number of characters in newly encoded representation.
calculateEncodedLength <- function(s) {
  encodedString <- s
  encodedString <- gsub('\\\\', '\\\\\\\\', encodedString)
  encodedString <- gsub('\"', '\\\\\\\"', encodedString)
  encodedString <- paste0('\"', encodedString, '\"')
  return(nchar(encodedString))
}

#' Solves Part One: computes difference between total code representation length and in-memory length.
#'
#' @return Difference in characters for Part One.
solvePartOne <- function() {
  totalCodeLength <- sum(sapply(data, calculateCodeLength))
  totalMemoryLength <- sum(sapply(data, calculateMemoryLength))
  return(totalCodeLength - totalMemoryLength)
}

#' Solves Part Two: computes difference between total newly encoded length and code representation length.
#'
#' @return Difference in characters for Part Two.
solvePartTwo <- function() {
  totalCodeLength <- sum(sapply(data, calculateCodeLength))
  totalEncodedLength <- sum(sapply(data, calculateEncodedLength))
  return(totalEncodedLength - totalCodeLength)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')