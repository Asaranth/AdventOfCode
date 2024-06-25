data <- readLines('./2015/data/01.txt')

solvePartOne <- function() {
  ups <- stringr::str_count(data, '\\(')
  downs <- stringr::str_count(data, '\\)')
  return(ups - downs)
}

solvePartTwo <- function() {
  input_data <- strsplit(data, '')[[1]]
  index_vector <- seq_along(input_data)
  floor_vector <- cumsum(ifelse(input_data =='(', 1, -1))
  return(min(index_vector[floor_vector == -1]))
}

stringr::str_interp('Part One: ${solvePartOne()}')
stringr::str_interp('Part Two: ${solvePartTwo()}')