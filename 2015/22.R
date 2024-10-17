source(file.path(getwd(), '2015/utils.R'))
data <- getInputData(22)

boss <- list(
  hp = as.numeric(strsplit(data[1], ':')[[1]][2]),
  damage = as.numeric(strsplit(data[2], ':')[[1]][2])
)

player <- list(hp = 50, mana = 500)

spells <- data.frame(
  name = c('Magic Missile', 'Drain', 'Shield', 'Poison', 'Recharge'),
  cost = c(53, 73, 113, 173, 229),
  damage = c(4, 2, 0, 0, 0),
  heal = c(0, 2, 0, 0, 0),
  duration = c(0, 0, 6, 6, 5)
)

minMana <<- Inf
memo <- list()

simulate <- function(player, boss, hardMode = FALSE, maxIterations = 100000) {
  effects <- list(shield = 0, poison = 0, recharge = 0)

  hashState <- function(state) {
    paste(
      state$player$hp, state$player$mana, state$boss$hp,
      state$effects$shield, state$effects$poison, state$effects$recharge,
      state$manaSpent, state$turn, sep = '-'
    )
  }

  applyEffects <- function(player, boss, effects) {
    if (effects$shield > 0) {
      player$armor <- 7
    } else {
      player$armor <- 0
    }

    if (effects$poison > 0) {
      boss$hp <- boss$hp - 3
    }

    if (effects$recharge > 0) {
      player$mana <- player$mana + 101
    }

    effects <- lapply(effects, function(x) max(0, x - 1))

    list(player = player, boss = boss, effects = effects)
  }

  simulateTurn <- function(player, boss, effects, spell = NULL) {
    if (!is.null(spell)) {
      if (player$mana < spell$cost) {
        return(NULL)
      }
      player$mana <- player$mana - spell$cost

      boss$hp <- boss$hp - spell$damage
      player$hp <- min(50, player$hp + spell$heal)

      if (spell$duration > 0) {
        if (spell$name == 'Shield') effects$shield <- spell$duration
        if (spell$name == 'Poison') effects$poison <- spell$duration
        if (spell$name == 'Recharge') effects$recharge <- spell$duration
      }
    }

    list(player = player, boss = boss, effects = effects)
  }

  iterations <- 0
  stack <- list(list(player = player, boss = boss, effects = effects, manaSpent = 0, turn = 'player'))

  while (length(stack) > 0) {
    iterations <- iterations + 1
    if (iterations > maxIterations) {
      cat('Max iterations reached, breaking loop.\n')
      break
    }

    state <- stack[[1]]
    stack <- stack[-1]

    stateHash <- hashState(state)
    if (!is.null(memo[[stateHash]]) && memo[[stateHash]] <= state$manaSpent) next
    memo[[stateHash]] <- state$manaSpent

    if (!is.null(minMana) && state$manaSpent >= minMana) next

    if (state$turn == 'player') {
      if (hardMode) {
        state$player$hp <- state$player$hp - 1
        if (state$player$hp <= 0) next
      }

      tmp <- applyEffects(state$player, state$boss, state$effects)
      state$player <- tmp$player
      state$boss <- tmp$boss
      state$effects <- tmp$effects

      if (state$boss$hp <= 0) {
        minMana <<- min(minMana, state$manaSpent)
        next
      }

      for (i in seq_len(nrow(spells))) {
        if (state$player$mana < spells$cost[i]) next

        spell <- as.list(spells[i,])

        effectActive <- switch(spell$name,
                               'Shield' = state$effects$shield > 0,
                               'Poison' = state$effects$poison > 0,
                               'Recharge' = state$effects$recharge > 0,
                               FALSE)

        if (spell$duration > 0 && effectActive) next

        tmp <- simulateTurn(state$player, state$boss, state$effects, spell)

        if (is.null(tmp)) next

        stack <- c(
          stack,
          list(list(
            player = tmp$player,
            boss = tmp$boss,
            effects = tmp$effects,
            manaSpent = state$manaSpent + spell$cost,
            turn = 'boss'
          ))
        )
      }
    } else {
      tmp <- applyEffects(state$player, state$boss, state$effects)
      state$player <- tmp$player
      state$boss <- tmp$boss
      state$effects <- tmp$effects

      if (state$boss$hp <= 0) {
        minMana <<- min(minMana, state$manaSpent)
        next
      }

      state$player$hp <- state$player$hp - max(1, state$boss$damage - state$player$armor)
      if (state$player$hp <= 0) next

      stack <- c(
        stack,
        list(list(
          player = state$player,
          boss = state$boss,
          effects = state$effects,
          manaSpent = state$manaSpent,
          turn = 'player'
        ))
      )
    }
  }

  minMana
}

solvePartOne <- function() {
  minMana <<- Inf
  memo <<- list()
  simulate(player, boss, hardMode = FALSE, maxIterations = 200000)
}

solvePartTwo <- function() {
  minMana <<- Inf
  memo <<- list()
  simulate(player, boss, hardMode = TRUE, maxIterations = 100000)
}

cat('Part One:', solvePartOne(), '\n')
cat('Part Two:', solvePartTwo(), '\n')