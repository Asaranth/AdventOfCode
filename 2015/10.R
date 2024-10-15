data <- readLines('./2015/data/10.txt')

nextSequence <- function(sequence) {
  rleSequence <- rle(strsplit(sequence, NULL)[[1]])
  paste(mapply(function(times, digit) paste0(times, digit), rleSequence$lengths, rleSequence$values), collapse = '')
}

solvePartOne <- function() {
  sequence <- data
  for (i in 1:40) {
    sequence <- nextSequence(sequence)
  }
  return(list(result = nchar(sequence), sequence = sequence))
}

solvePartTwo <- function(sequence) {
  for (i in 1:10) {
    sequence <- nextSequence(sequence)
  }
  return(nchar(sequence))
}

partOneResult <- solvePartOne()
cat('Part One:', partOneResult$result, '\n')
cat('Part Two:', solvePartTwo(partOneResult$sequence), '\n')