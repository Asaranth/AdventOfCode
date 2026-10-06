#' Day 10: Elves Look, Elves Say
#'
#' Generates Conway look-and-say sequences via run-length encoding.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(10)

#' Generates the next look-and-say sequence using run-length encoding.
#'
#' @param sequence Input digit string.
#' @return Resulting look-and-say digit string.
nextSequence <- function(sequence) {
  rleSequence <- rle(strsplit(sequence, NULL)[[1]])
  paste(mapply(function(times, digit) paste0(times, digit), rleSequence$lengths, rleSequence$values), collapse = '')
}

#' Solves Part One: applies 40 look-and-say iterations and measures sequence length.
#'
#' @return A list containing the character length and sequence string after 40 iterations.
solvePartOne <- function() {
  sequence <- data
  for (i in 1:40) {
    sequence <- nextSequence(sequence)
  }
  return(list(result = nchar(sequence), sequence = sequence))
}

#' Solves Part Two: applies 10 additional iterations (50 total) and returns final length.
#'
#' @param sequence Sequence string resulting from Part One (after 40 iterations).
#' @return Final sequence length after 50 iterations.
solvePartTwo <- function(sequence) {
  for (i in 1:10) {
    sequence <- nextSequence(sequence)
  }
  return(nchar(sequence))
}

partOneResult <- solvePartOne()
cat('Part One:', partOneResult$result, '\n')
cat('Part Two:', solvePartTwo(partOneResult$sequence), '\n')