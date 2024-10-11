data <- readLines("./2015/data/07.txt")
wires <- new.env()

COMMAND_REGEX <- "[A-Z]+"
ARGUMENTS_REGEX <- "[a-z0-9]+"

BITWISE_METHODS <- list(
  AND = function(a, b) bitwAnd(a, b),
  OR = function(a, b) bitwOr(a, b),
  NOT = function(a) bitwNot(a),
  LSHIFT = function(a, b) bitwShiftL(a, b),
  RSHIFT = function(a, b) bitwShiftR(a, b)
)

parseInstruction <- function(instruction) {
  command <- regmatches(instruction, gregexpr(COMMAND_REGEX, instruction))[[1]]
  args <- regmatches(instruction, gregexpr(ARGUMENTS_REGEX, instruction))[[1]]
  destination <- tail(args, n = 1)
  args <- args[-length(args)]

  args <- lapply(args, function(arg) {
    if (grepl("^\\d+$", arg)) {
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

solvePartOne <- function() {
  for (instruction in data) {
    parsedInstruction <- parseInstruction(instruction)
    wires[[parsedInstruction$destination]] <- list(command = parsedInstruction$command, args = parsedInstruction$args)
  }
  return(calculateWire('a', wires))
}

solvePartTwo <- function(a) {
  for (instruction in data) {
    parsedInstruction <- parseInstruction(instruction)
    wires[[parsedInstruction$destination]] <- list(command = parsedInstruction$command, args = parsedInstruction$args)
  }
  wires[['b']] <- a
  return(calculateWire('a', wires))
}

partOneResult <- solvePartOne()
stringr::str_interp('Part One: ${partOneResult}')
stringr::str_interp('Part Two: ${solvePartTwo(partOneResult)}')