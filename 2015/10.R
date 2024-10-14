data <- readLines('./2015/data/10.txt')

next_sequence <- function(sequence) {
  rle_sequence <- rle(strsplit(sequence, NULL)[[1]])
  paste(mapply(function(times, digit) paste0(times, digit), rle_sequence$lengths, rle_sequence$values), collapse = '')
}

solvePartOne <- function() {
  sequence <- data
  for (i in 1:40) {
    sequence <- next_sequence(sequence)
  }
  return(list(result = nchar(sequence), sequence = sequence))
}

solvePartTwo <- function(sequence) {
  for (i in 1:10) {
    sequence <- next_sequence(sequence)
  }
  return(nchar(sequence))
}

partOneResult <- solvePartOne()
cat(stringr::str_interp('Part One: ${partOneResult$result}\n'))
cat(stringr::str_interp('Part Two: ${solvePartTwo(partOneResult$sequence)}\n'))