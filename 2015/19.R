source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(19)

parseInput <- function(data) {
  replacements <- list()
  molecule <- NULL
  for (line in data) {
    if (grepl('=>', line)) {
      parts <- unlist(strsplit(line, ' => '))
      replacements[[length(replacements) + 1]] <- c(parts[1], parts[2])
    } else if (line != '') {
      molecule <- line
    }
  }
  return(list(replacements = replacements, molecule = molecule))
}

countStepsToSolve <- function(molecule) {
  elements <- gregexpr('[A-Z][a-z]?', molecule, perl = TRUE)[[1]]
  numElements <- length(elements)
  numRn <- length(gregexpr('Rn', molecule, perl = TRUE)[[1]])
  numAr <- length(gregexpr('Ar', molecule, perl = TRUE)[[1]])
  numY <- length(gregexpr('Y', molecule, perl = TRUE)[[1]])
  steps <- numElements - numRn - numAr - 2 * numY - 1
  return(steps)
}

solvePartOne <- function() {
  parsedData <- parseInput(data)
  replacements <- parsedData$replacements
  molecule <- parsedData$molecule
  distinctMolecules <- character()
  for (replacement in replacements) {
    original <- replacement[1]
    replacementText <- replacement[2]
    start_pos <- gregexpr(pattern = original, text = molecule)[[1]]
    for (pos in start_pos) {
      if (pos != -1) {
        newMolecule <- paste0(
          substring(molecule, 1, pos - 1),
          replacementText,
          substring(molecule, pos + nchar(original))
        )
        distinctMolecules <- c(distinctMolecules, newMolecule)
      }
    }
  }
  distinctMolecules <- unique(distinctMolecules)
  return(length(distinctMolecules))
}

solvePartTwo <- function() {
  parsedData <- parseInput(data)
  molecule <- parsedData$molecule
  steps <- countStepsToSolve(molecule)
  return(steps)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')