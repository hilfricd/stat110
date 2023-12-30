 # simulate one round of gambler's ruin

i <- 10
N <- 20
p_a <- 0.5

n_rounds <- 0
while(i > 0 & i < N) {
  # next bet
  result <- sample(c(1, -1), 1, prob = c(p_a, 1-p_a))

  i <- i + result
  n_rounds <- n_rounds + 1
}
