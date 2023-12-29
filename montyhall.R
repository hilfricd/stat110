
## Simulates the Monty-Hall-Cardoor-Problem n times
simulate_monty_hall <- function(n = 1000, switch = TRUE) {
  if(!switch) {
    cardoor <- sample(3, n, replace = TRUE)
    wins <- sum(cardoor == 1)
  }

  print(paste0("Monty Hall Problem simulated ", n, " times!"))
  print(paste0("Strategy: ", ifelse(switch, "Switching", "Never switching")))
}
