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
  geom_col(color = "white", fill = "skyblue", alpha = 0.8) +
  geom_col(aes(y = pmf),
           color = "darkblue", fill = NA, alpha = 0.8) +
  scale_x_continuous(breaks = 0:trials) +
  theme_classic()
