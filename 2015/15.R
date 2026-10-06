#' Day 15: Science for Hungry People
#'
#' Optimises cookie ingredient proportions across capacity, durability, flavour, texture, and calories.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(15)

#' Parses ingredient descriptions into property score vectors.
#'
#' @param data Character vector of ingredient specification strings.
#' @return Named list of property score vectors for each ingredient.
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

#' Computes cookie recipe score across properties, with optional calorie restriction.
#'
#' @param amounts Named list of teaspoon quantities for each ingredient.
#' @param ingredients Named list of ingredient property vectors.
#' @param calorieConstraint Logical flag requiring exactly 500 total calories.
#' @return Computed recipe score (product of non-negative property totals).
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

#' Searches all 100-teaspoon combinations to find the highest achievable recipe score.
#'
#' @param ingredients Named list of ingredient property vectors.
#' @param ingredientNames Character vector of ingredient names.
#' @param calorieConstraint Logical flag requiring exactly 500 total calories.
#' @return Maximum achievable recipe score.
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

#' Solves Part One: finds highest cookie score without calorie restrictions.
#'
#' @return Best cookie score for Part One.
solvePartOne <- function() {
  findBestScore(ingredients, ingredientNames, calorieConstraint = FALSE)
}

#' Solves Part Two: finds highest cookie score with a 500-calorie constraint.
#'
#' @return Best cookie score for Part Two.
solvePartTwo <- function() {
  findBestScore(ingredients, ingredientNames, calorieConstraint = TRUE)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')