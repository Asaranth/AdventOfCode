#' Day 07: Some Assembly Required
#'
#' Emulates a 16-bit logic circuit with memoised wire signal propagation.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(7)
wires <- new.env()

COMMAND_REGEX <- '[A-Z]+'
ARGUMENTS_REGEX <- '[a-z0-9]+'

BITWISE_METHODS <- list(
  AND = function(a, b) bitwAnd(a, b),
  OR = function(a, b) bitwOr(a, b),
  NOT = function(a) bitwNot(a),
  LSHIFT = function(a, b) bitwShiftL(a, b),
  RSHIFT = function(a, b) bitwShiftR(a, b)
)

#' Parses a wiring instruction into operation command, argument list, and destination wire.
#'
#' @param instruction Raw instruction string.
#' @return A list containing command name, arguments, and destination wire identifier.
parseInstruction <- function(instruction) {
  command <- regmatches(instruction, gregexpr(COMMAND_REGEX, instruction))[[1]]
  args <- regmatches(instruction, gregexpr(ARGUMENTS_REGEX, instruction))[[1]]
  destination <- tail(args, n = 1)
  args <- args[-length(args)]

  args <- lapply(args, function(arg) {
    if (grepl('^\\d+$', arg)) {
      as.numeric(arg)
    } else {
      arg
    }
  })

  if (length(command) == 0) {
    command <- NULL
  }

  list(command = command, args = args, destination = destination)
}

#' Recursively evaluates the signal on a specified wire using memoised environment storage.
#'
#' @param wireName Wire identifier string or numeric literal.
#' @param wires Environment storing wire definitions and cached numeric values.
#' @return Evaluated 16-bit integer signal on the wire.
calculateWire <- function(wireName, wires) {
  if (is.numeric(wireName)) return(wireName)
  wire <- wires[[wireName]]

  if (is.numeric(wire)) return(wire)
  if (is.null(wire)) return(NULL)

  if (is.null(wire$command)) {
    wires[[wireName]] <- calculateWire(wire$args[[1]], wires)
  } else {
    wires[[wireName]] <- do.call(BITWISE_METHODS[[wire$command]], lapply(wire$args, calculateWire, wires = wires))
  }

  return(wires[[wireName]])
}

#' Solves Part One: computes the signal provided to wire 'a'.
#'
#' @return Signal value on wire 'a'.
solvePartOne <- function() {
  for (instruction in data) {
    parsedInstruction <- parseInstruction(instruction)
    wires[[parsedInstruction$destination]] <- list(command = parsedInstruction$command, args = parsedInstruction$args)
  }
  return(calculateWire('a', wires))
}

#' Solves Part Two: overrides wire 'b' with the result of Part One and re-evaluates wire 'a'.
#'
#' @param a Output signal from Part One.
#' @return Updated signal value on wire 'a'.
solvePartTwo <- function(a) {
  for (instruction in data) {
    parsedInstruction <- parseInstruction(instruction)
    wires[[parsedInstruction$destination]] <- list(command = parsedInstruction$command, args = parsedInstruction$args)
  }
  wires[['b']] <- a
  return(calculateWire('a', wires))
}

partOneResult <- solvePartOne()
cat('Part One:', partOneResult, '\n')
cat('Part Two:', solvePartTwo(partOneResult), '\n')