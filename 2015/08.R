source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(8)

calculateCodeLength <- function(s) {
  return(nchar(s))
}

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

calculateEncodedLength <- function(s) {
  encodedString <- s
  encodedString <- gsub('\\\\', '\\\\\\\\', encodedString)
  encodedString <- gsub('\"', '\\\\\\\"', encodedString)
  encodedString <- paste0('\"', encodedString, '\"')
  return(nchar(encodedString))
}

solvePartOne <- function() {
  totalCodeLength <- sum(sapply(data, calculateCodeLength))
  totalMemoryLength <- sum(sapply(data, calculateMemoryLength))
  return(totalCodeLength - totalMemoryLength)
}

solvePartTwo <- function() {
  totalCodeLength <- sum(sapply(data, calculateCodeLength))
  totalEncodedLength <- sum(sapply(data, calculateEncodedLength))
  return(totalEncodedLength - totalCodeLength)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')