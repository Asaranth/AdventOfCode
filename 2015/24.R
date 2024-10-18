source(file.path(getwd(), '2015/utils.R'))
data <- as.numeric(getInputData(24))

findCombinations <- function(data, target) {
  for (i in seq_along(data)) {
    combs <- combn(data, i)
    colSumsCombs <- colSums(combs)
    if (any(colSumsCombs == target)) {
      return(combs[, which(colSumsCombs == target)])
    }
  }
  return(NULL)
}

findOptimalGroup <- function(data, groups) {
  target <- sum(data) / groups
  firstGroup <- findCombinations(data, target)

  if (is.null(firstGroup)) {
    stop('No valid group found')
  }

  remainingData <- setdiff(data, firstGroup[, 1])
  findCombinations(remainingData, target)

  qe <- apply(firstGroup, 2, prod)
  index <- order(qe)[1]

  return(qe[index])
}

solvePartOne <- function() {
  return(findOptimalGroup(data, 3))
}

solvePartTwo <- function() {
  return(findOptimalGroup(data, 4))
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')