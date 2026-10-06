#' Day 23: Opening the Turing Lock
#'
#' Simulates a custom assembly register machine with jumps, halving, tripling, and increment instructions.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(23)

#' Executes instructions on registers 'a' and 'b' until program termination.
#'
#' @param a Initial integer value for register 'a'.
#' @param b Initial integer value for register 'b'.
#' @return Final integer value in register 'b'.
execute <- function(a, b) {
  pc <- 1
  while (pc <= length(data) && pc > 0) {
    instruction <- data[pc]
    parts <- unlist(strsplit(instruction, ' '))
    cmd <- parts[1]
    if (cmd %in% c('hlf', 'tpl', 'inc')) {
      reg <- substr(parts[2], 1, 1)
      if (cmd == 'hlf') {
        if (reg == 'a') a <- a / 2 else b <- b / 2
      } else if (cmd == 'tpl') {
        if (reg == 'a') a <- a * 3 else b <- b * 3
      } else if (cmd == 'inc') {
        if (reg == 'a') a <- a + 1 else b <- b + 1
      }
      pc <- pc + 1
    } else if (cmd == 'jmp') {
      offset <- as.numeric(parts[2])
      pc <- pc + offset
    } else if (cmd == 'jie' || cmd == 'jio') {
      reg <- substr(parts[2], 1, 1)
      offset <- as.numeric(parts[3])
      if (cmd == 'jie' && ((reg == 'a' && a %% 2 == 0) || (reg == 'b' && b %% 2 == 0))) {
        pc <- pc + offset
      } else if (cmd == 'jio' && ((reg == 'a' && a == 1) || (reg == 'b' && b == 1))) {
        pc <- pc + offset
      } else {
        pc <- pc + 1
      }
    } else {
      stop('Unknown instruction: ', cmd)
    }
  }
  return(b)
}

#' Solves Part One: runs the program with register 'a' initialised to 0 and returns register 'b'.
#'
#' @return Final register 'b' value for Part One.
solvePartOne <- function() {
  return(execute(0, 0))
}

#' Solves Part Two: runs the program with register 'a' initialised to 1 and returns register 'b'.
#'
#' @return Final register 'b' value for Part Two.
solvePartTwo <- function() {
  return(execute(1, 0))
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')