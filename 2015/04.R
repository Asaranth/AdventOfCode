data <- readLines('./2015/data/04.txt', n=1)

solvePartOne <- function() {
  number <- seq_len(400000)
  md5 <- digest::getVDigest()
  hash <- vapply(paste0(data, number), md5, FUN.VALUE=character(1), serialize=FALSE)
  return(which(substr(hash, 1, 5) == "00000"))
}

solvePartTwo <- function() {
  number <- seq(9000000, 10000000)
  md5 <- digest::getVDigest()
  hash <- vapply(paste0(data, number), md5, FUN.VALUE=character(1), serialize=FALSE)
  return(number[[which(substr(hash, 1, 6) == "000000")]])
}

cat(stringr::str_interp('Part One: ${solvePartOne()}\n'))
cat(stringr::str_interp('Part Two: ${solvePartTwo()}\n'))