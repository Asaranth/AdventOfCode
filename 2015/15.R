data <- readLines('./2015/data/15.txt')

parseIngredients <- function(data) {
  ingredients <- list()
  for (line in data) {
    parts <- unlist(strsplit(line, ': |, '))
    name <- parts[1]
    properties <- as.numeric(sapply(parts[2:length(parts)], function(x) gsub('[^0-9-]', '', x)))
    ingredients[[name]] <- setNames(properties, c("capacity", "durability", "flavor", "texture", "calories"))
  }
  return(ingredients)
}

ingredients <- parseIngredients(data)
ingredientNames <- names(ingredients)

calculateScore <- function(amounts, ingredients, calorieConstraint = FALSE) {
  totalCapacity <- 0
  totalDurability <- 0
  totalFlavor <- 0
  totalTexture <- 0
  totalCalories <- 0

  for (name in ingredientNames) {
    amount <- amounts[[name]]
    ingredient <- ingredients[[name]]
    totalCapacity <- totalCapacity + amount * ingredient["capacity"]
    totalDurability <- totalDurability + amount * ingredient["durability"]
    totalFlavor <- totalFlavor + amount * ingredient["flavor"]
    totalTexture <- totalTexture + amount * ingredient["texture"]
    totalCalories <- totalCalories + amount * ingredient["calories"]
  }

  if (calorieConstraint && totalCalories != 500) {
    return(0)
  }

  return(max(0, totalCapacity) * max(0, totalDurability) * max(0, totalFlavor) * max(0, totalTexture))
}

findBestScore <- function(ingredients, ingredientNames, calorieConstraint = FALSE) {
  bestScore <- 0
  combinations <- expand.grid(rep(list(0:100), length(ingredientNames)))
  validCombinations <- combinations[rowSums(combinations) == 100, ]

  for (i in seq_len(nrow(validCombinations))) {
    amounts <- setNames(as.list(validCombinations[i, ]), ingredientNames)
    score <- calculateScore(amounts, ingredients, calorieConstraint)
    if (score > bestScore) {
      bestScore <- score
    }
  }

  return(bestScore)
}

solvePartOne <- function() {
  findBestScore(ingredients, ingredientNames, calorieConstraint = FALSE)
}

solvePartTwo <- function() {
  findBestScore(ingredients, ingredientNames, calorieConstraint = TRUE)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')