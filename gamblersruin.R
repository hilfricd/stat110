library(dplyr)
library(ggplot2)
library(ggpmisc)

# simulates one round of gambler's ruin
# i: wealth of player A at start of game
# N: total wealth in the game (i + wealth of player B)
# p_a: probability of A winning each individual bet
# Returns the wealth of player A after each bet
simulate_gamblers_ruin <- function(i, N, p_a = 0.5) {
  wealth_a <- i
  while(i > 0 & i < N) {
    # next bet
    result <- sample(c(1, -1), 1, prob = c(p_a, 1-p_a))

    i <- i + result
    wealth_a <- c(wealth_a, i)
  }
  return(wealth_a)
}

# plots results of one gambler's ruin game
plot_gamblers_ruin <- function(x) {
  # turn results into tibble for ggplot2
  results <- tibble(
    bet_no = 0:(length(x)-1),
    wealth_a = x
  )

  # create plot
  ggplot(results, aes(bet_no, wealth_a)) +
    geom_step() +
    scale_y_continuous(limits = c(0, (results[1,] %>% pull(wealth_a))*2)) +
    theme_classic() +
    labs(x = "Bet No.", y = "Wealth of player A")
}


simulate_multiple_gamblers_ruins <- function(n_games, i, N, p_a = 0.5) {
  results <- tribble(~game_no, ~a_won, ~no_bets)

  for(game in 1:n_games) {
    round_results <- simulate_gamblers_ruin(i, N, p_a)

    results <- results %>%
      add_row(
        game_no = game,
        a_won = last(round_results) != 0,
        no_bets = length(round_results) - 1
      )
  }
  return(results)
}

results <- simulate_multiple_gamblers_ruins(1000, 100, 200, 0.49)

results %>%
  summarize(
    no_wins = sum(a_won),
    frac_wins = round(sum(a_won) / n(), 2),
    mean_bets = round(mean(no_bets), 2),
    sd_bets = round(sd(no_bets), 2)
  )

ggplot(results, aes(no_bets)) +
  geom_histogram()
