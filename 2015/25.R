source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(25)

numbers <- unlist(regmatches(data, gregexpr("\\d+", data)))
numbers <- as.numeric(numbers)

if (length(numbers) >= 2) {
  targetRow <- numbers[1]
  targetCol <- numbers[2]
} else {
  stop("Failed to extract row and column numbers from input data.")
}

value <- 20151125
x <- 1
y <- 1

repeat {
  if (y == 1) {
    y <- x + 1
    x <- 1
  } else {
    y <- y - 1
    x <- x + 1
  }
  value <- (value * 252533) %% 33554393
  if (x == targetCol & y == targetRow) break
}

cat('Solution:', value, '\n')