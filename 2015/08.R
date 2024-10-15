data <- readLines('./2015/data/08.txt')

calculate_code_length <- function(s) {
  return(nchar(s))
}

calculate_memory_length <- function(s) {
  s <- substring(s, 2, nchar(s) - 1)
  i <- 1
  in_memory_length <- 0
  while (i <= nchar(s)) {
    char <- substring(s, i, i)
    if (char == '\\') {
      next_char <- substring(s, i+1, i+1)
      if (next_char %in% c('\\', '\"')) {
        in_memory_length <- in_memory_length + 1
        i <- i + 2
      } else if (next_char == 'x') {
        in_memory_length <- in_memory_length + 1
        i <- i + 4
      } else {
        in_memory_length <- in_memory_length + 1
        i <- i + 1
      }
    } else {
      in_memory_length <- in_memory_length + 1
      i <- i + 1
    }
  }
  return(in_memory_length)
}

calculate_encoded_length <- function(s) {
  encoded_string <- s
  encoded_string <- gsub('\\\\', '\\\\\\\\', encoded_string)
  encoded_string <- gsub('\"', '\\\\\\\"', encoded_string)
  encoded_string <- paste0('\"', encoded_string, '\"')
  return(nchar(encoded_string))
}

solvePartOne <- function() {
  total_code_length <- sum(sapply(data, calculate_code_length))
  total_memory_length <- sum(sapply(data, calculate_memory_length))
  return(total_code_length - total_memory_length)
}

solvePartTwo <- function() {
  total_code_length <- sum(sapply(data, calculate_code_length))
  total_encoded_length <- sum(sapply(data, calculate_encoded_length))
  return(total_encoded_length - total_code_length)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')