library(dplyr)
library(ggplot2)

#################################
#       BINOMIAL                #
#################################

trials <- 10
p <- 0.5

rv_bin <- tibble(
  k = rbinom(10000, n, p)) %>%
  count(k) %>%
  mutate(frac = n/sum(n)) %>%
  mutate(pmf = dbinom(0:trials, trials, p))

ggplot(rv_bin, aes(k, frac)) +
  geom_col() +
  geom_step(aes(y = pmf)) +
  scale_x_continuous(breaks = 0:trials)
