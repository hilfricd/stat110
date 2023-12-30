library(dplyr)

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

testrun <- simulate_gamblers_ruin(10, 20, 0.5)
testresults <- tibble(
  bet_no = 0:(length(testrun)-1),
  wealth_a = testrun
)

