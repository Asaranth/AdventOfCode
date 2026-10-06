#' Day 21: RPG Simulator 20XX
#'
#' Simulates turn-based RPG combat across weapon, armour, and ring equipment combinations.

source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(21)

boss <- data.frame(
  hp = as.numeric(strsplit(data[1], ':')[[1]][2]),
  damage = as.numeric(strsplit(data[2], ':')[[1]][2]),
  armor = as.numeric(strsplit(data[3], ':')[[1]][2])
)

playerHp <- 100

weapons <- data.frame(
  item = c('Dagger', 'Shortsword', 'Warhammer', 'Longsword', 'Greataxe'),
  cost = c(8, 10, 25, 40, 74),
  damage = c(4, 5, 6, 7, 8)
)

armors <- data.frame(
  item = c('None', 'Leather', 'Chainmail', 'Splintmail', 'Bandedmail', 'Platemail'),
  cost = c(0, 13, 31, 53, 75, 102),
  armor = c(0, 1, 2, 3, 4, 5)
)

rings <- data.frame(
  item = c('None', 'Damage +1', 'Damage +2', 'Damage +3', 'Defense +1', 'Defense +2', 'Defense +3'),
  cost = c(0, 25, 50, 100, 20, 40, 80),
  damage = c(0, 1, 2, 3, 0, 0, 0),
  armor = c(0, 0, 0, 0, 1, 2, 3)
)

#' Simulates turn-based combat between player and boss until one is defeated.
#'
#' @param playerDamage Player's total damage stat.
#' @param playerArmor Player's total armour stat.
#' @return TRUE if player wins; otherwise, FALSE.
simulate <- function(playerDamage, playerArmor) {
  bossHpLeft <- boss$hp
  playerHpLeft <- playerHp
  repeat {
    damageToBoss <- max(1, playerDamage - boss$armor)
    bossHpLeft <- bossHpLeft - damageToBoss
    if (bossHpLeft <= 0) {
      return(TRUE)
    }

    damageToPlayer <- max(1, boss$damage - playerArmor)
    playerHpLeft <- playerHpLeft - damageToPlayer
    if (playerHpLeft <= 0) {
      return(FALSE)
    }
  }
}

#' Computes total gold cost, damage, and armour for an equipment loadout.
#'
#' @param weapon 1-based index into weapons table.
#' @param armor 1-based index into armours table.
#' @param ring1 1-based index into rings table for first finger.
#' @param ring2 1-based index into rings table for second finger.
#' @return A list containing total cost, damage, and armour stats.
calculateStats <- function(weapon, armor, ring1, ring2) {
  totalCost <- sum(weapons$cost[weapon], armors$cost[armor], rings$cost[ring1], rings$cost[ring2])
  totalDamage <- sum(weapons$damage[weapon], rings$damage[ring1], rings$damage[ring2])
  totalArmor <- sum(armors$armor[armor], rings$armor[ring1], rings$armor[ring2])
  return(list(cost = totalCost, damage = totalDamage, armor = totalArmor))
}

#' Iterates through all legal item combinations, invoking a callback function for each.
#'
#' @param callback Function accepting (weapon, armor, ring1, ring2) index arguments.
forEachCombination <- function(callback) {
  for (weapon in seq_len(nrow(weapons))) {
    for (armor in seq_len(nrow(armors))) {
      for (ring1 in seq_len(nrow(rings))) {
        for (ring2 in seq_len(nrow(rings))) {
          if (ring1 != 1 && ring2 != 1 && ring1 == ring2) next
          callback(weapon, armor, ring1, ring2)
        }
      }
    }
  }
}

#' Finds the minimum gold spend required for an equipment set that defeats the boss.
#'
#' @return Minimum gold cost to achieve victory.
findMinCostToWin <- function() {
  minCost <<- Inf
  forEachCombination(function(weapon, armor, ring1, ring2) {
    stats <- calculateStats(weapon, armor, ring1, ring2)
    if (simulate(stats$damage, stats$armor) && stats$cost < minCost) {
      minCost <<- stats$cost
    }
  })
  return(minCost)
}

#' Finds the maximum gold spend on an equipment set that still results in defeat.
#'
#' @return Maximum gold cost while losing.
findMaxCostToLose <- function() {
  maxCost <<- -Inf
  forEachCombination(function(weapon, armor, ring1, ring2) {
    stats <- calculateStats(weapon, armor, ring1, ring2)
    if (!simulate(stats$damage, stats$armor) && stats$cost > maxCost) {
      maxCost <<- stats$cost
    }
  })
  return(maxCost)
}

#' Solves Part One: finds minimum equipment cost to win the fight.
#'
#' @return Minimum cost to win.
solvePartOne <- function() {
  return(findMinCostToWin())
}

#' Solves Part Two: finds maximum equipment cost to still lose the fight.
#'
#' @return Maximum cost to lose.
solvePartTwo <- function() {
  return(findMaxCostToLose())
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')