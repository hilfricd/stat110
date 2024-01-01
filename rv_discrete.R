library(dplyr)
library(tidyr)
library(ggplot2)

#################################
#       BINOMIAL                #
#################################

plot_binom_pmf <- function(trials, p) {
  rv_bin <- tibble(
    k = 0:trials,
    pmf = dbinom(0:trials, trials, p)) %>%
    left_join(
      tibble(k = rbinom(10000, trials, p)) %>% count(k), by = "k") %>%
    mutate(
      n = replace_na(n, 0),
      frac = n/sum(n)
    )

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
plot_binom_pmf(20, 0.3)
