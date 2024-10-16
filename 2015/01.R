source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(1)

solvePartOne <- function() {
  ups <- stringr::str_count(data, '\\(')
  downs <- stringr::str_count(data, '\\)')
  return(ups - downs)
}

solvePartTwo <- function() {
  inputData <- strsplit(data, '')[[1]]
  indexVector <- seq_along(inputData)
  floorVector <- cumsum(ifelse(inputData == '(', 1, -1))
  return(min(indexVector[floorVector == -1]))
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')