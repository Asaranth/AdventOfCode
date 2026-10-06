#' Day 04: The Ideal Stocking Stuffer
#'
#' Mines MD5 hash prefixes to discover lowest positive integer nonces yielding five and six leading zeroes.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(4)

#' Solves Part One: finds the lowest integer producing an MD5 hash with at least five leading zeroes.
#'
#' @return The lowest matching integer nonce.
solvePartOne <- function() {
  number <- seq_len(400000)
  md5 <- digest::getVDigest()
  hash <- vapply(paste0(data, number), md5, FUN.VALUE=character(1), serialize=FALSE)
  return(which(substr(hash, 1, 5) == "00000"))
}

#' Solves Part Two: finds the lowest integer producing an MD5 hash with at least six leading zeroes.
#'
#' @return The lowest matching integer nonce.
solvePartTwo <- function() {
  number <- seq(9000000, 10000000)
  md5 <- digest::getVDigest()
  hash <- vapply(paste0(data, number), md5, FUN.VALUE=character(1), serialize=FALSE)
  return(number[[which(substr(hash, 1, 6) == "000000")]])
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')