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

calculateStats <- function(weapon, armor, ring1, ring2) {
  totalCost <- sum(weapons$cost[weapon], armors$cost[armor], rings$cost[ring1], rings$cost[ring2])
  totalDamage <- sum(weapons$damage[weapon], rings$damage[ring1], rings$damage[ring2])
  totalArmor <- sum(armors$armor[armor], rings$armor[ring1], rings$armor[ring2])
  return(list(cost = totalCost, damage = totalDamage, armor = totalArmor))
}

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

solvePartOne <- function() {
  return(findMinCostToWin())
}

solvePartTwo <- function() {
  return(findMaxCostToLose())
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')