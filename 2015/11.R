#' Day 11: Corporate Policy
#'
#' Increments and validates passwords using sequential straight, exclusion, and non-overlapping pair checks.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(11)[1]
bannedLetters <- c('i', 'o', 'l')

#' Advances a password past banned characters by incrementing the first illegal letter and resetting trailing characters.
#'
#' @param pw Raw password string.
#' @return Adjusted password string free of early banned characters.
skipInvalidCharacters <- function(pw) {
  pw <- strsplit(pw, '')[[1]]
  for (i in seq_along(pw)) {
    if (pw[i] %in% bannedLetters) {
      pw[i] <- intToUtf8(utf8ToInt(pw[i]) + 1)
      for (j in (i + 1):length(pw)) {
        pw[j] <- 'a'
      }
      break
    }
  }
  paste(pw, collapse = "")
}

#' Increments a lowercase string alphabetically with base-26 wrap-around from right to left.
#'
#' @param password Current password string.
#' @return Lexicographically incremented password string.
incrementPassword <- function(password) {
  pw <- strsplit(password, '')[[1]]
  for (i in rev(seq_along(pw))) {
    if (pw[i] == 'z') {
      pw[i] <- 'a'
    } else {
      pw[i] <- intToUtf8(utf8ToInt(pw[i]) + 1)
      break
    }
  }
  paste(pw, collapse = '')
}

#' Checks whether a password contains at least one increasing straight of three letters.
#'
#' @param pw Password string.
#' @return TRUE if an increasing straight is present; otherwise, FALSE.
containsIncreasingStraight <- function(pw) {
  for (i in 1:(nchar(pw) - 2)) {
    if (utf8ToInt(substr(pw, i, i)) + 1 == utf8ToInt(substr(pw, i + 1, i + 1)) && utf8ToInt(substr(pw, i, i)) + 2 == utf8ToInt(substr(pw, i + 2, i + 2))) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Checks whether a password contains any banned letters ('i', 'o', or 'l').
#'
#' @param pw Password string.
#' @return TRUE if any banned letter is present; otherwise, FALSE.
containsBannedLetters <- function(pw) {
  return(any(charToRaw(pw) %in% charToRaw(paste(bannedLetters, collapse = ''))))
}

#' Checks whether a password contains at least two distinct, non-overlapping pairs of letters.
#'
#' @param pw Password string.
#' @return TRUE if two or more non-overlapping pairs are present; otherwise, FALSE.
containsTwoPairs <- function(pw) {
  pairs <- gregexpr('(.)\\1', pw)[[1]]
  if (pairs[1] == -1) return(FALSE)

  numPairs <- 0
  lastPos <- -2

  for (pos in pairs) {
    if (pos != lastPos + 1) numPairs <- numPairs + 1
    lastPos <- pos
  }

  return(numPairs >= 2)
}

#' Validates a password against all security criteria.
#'
#' @param pw Password string.
#' @return TRUE if the password satisfies all policy requirements; otherwise, FALSE.
isValidPassword <- function(pw) {
  containsIncreasingStraight(pw) && !containsBannedLetters(pw) && containsTwoPairs(pw)
}

#' Solves Part One: finds the next valid password after the puzzle input.
#'
#' @return The next valid password string.
solvePartOne <- function() {
  pw <- skipInvalidCharacters(data[1])
  repeat {
    pw <- incrementPassword(pw)
    if (isValidPassword(pw)) {
      return(pw)
    }
  }
}

#' Solves Part Two: finds the next valid password after Part One's result.
#'
#' @param pw Password string returned from Part One.
#' @return The second next valid password string.
solvePartTwo <- function(pw) {
  repeat {
    pw <- incrementPassword(pw)
    if (isValidPassword(pw)) {
      return(pw)
    }
  }
}

partOneResult <- solvePartOne()
cat('Part One:', partOneResult, '\n')
cat('Part Two:', solvePartTwo(partOneResult), '\n')