data <- readLines('./2015/data/11.txt')[1]
bannedLetters <- c('i', 'o', 'l')

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

containsIncreasingStraight <- function(pw) {
  for (i in 1:(nchar(pw) - 2)) {
    if (utf8ToInt(substr(pw, i, i)) + 1 == utf8ToInt(substr(pw, i + 1, i + 1)) && utf8ToInt(substr(pw, i, i)) + 2 == utf8ToInt(substr(pw, i + 2, i + 2))) {
      return(TRUE)
    }
  }
  return(FALSE)
}

containsBannedLetters <- function(pw) {
  return(any(charToRaw(pw) %in% charToRaw(paste(bannedLetters, collapse = ''))))
}

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

isValidPassword <- function(pw) {
  containsIncreasingStraight(pw) && !containsBannedLetters(pw) && containsTwoPairs(pw)
}

solvePartOne <- function() {
  pw <- skipInvalidCharacters(data[1])
  repeat {
    pw <- incrementPassword(pw)
    if (isValidPassword(pw)) {
      return(pw)
    }
  }
}

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