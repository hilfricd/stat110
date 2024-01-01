library(dplyr)
library(ggplot2)

#################################
#       BINOMIAL                #
#################################

trials <- 10
p <- 0.5


plot_binom_pmf <- function(trials, p) {
  rv_bin <- tibble(
    k = rbinom(10000, n, p)) %>%
    count(k) %>%
    mutate(frac = n/sum(n)) %>%
    mutate(pmf = dbinom(0:trials, trials, p))

  ggplot(rv_bin, aes(k, frac)) +
    geom_col(fill = "skyblue", alpha = 0.8) +
    geom_col(aes(y = pmf),
             color = "darkblue", fill = NA, alpha = 0.8) +
    scale_x_continuous(breaks = 0:trials) +
    theme_classic() +
    labs(title = paste0("Bin(", trials, ", ", p, ")"),
         subtitle = "Randomly sampled distribution (n=10,000, light blue),\nand corresponding PMF (dark blue)",
         y = "Relative frequency / probability")
}
