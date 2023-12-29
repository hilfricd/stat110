
## Simulates the Monty-Hall-Cardoor-Problem n times
simulate_monty_hall <- function(n = 1000, switch = TRUE, n_doors = 3) {
  doors <- 1:n_doors
  # randomly pick car door for each n
  cardoors <- sample(doors, n, replace = TRUE)

  if(!switch) {
    # if we don't switch we assume we always picked door 1
    wins <- sum(cardoors == 1)
  }
  else {
    # if we always switch we select the doors that monty opened,
    # then switch to the remaining door,
    # and count the number of times we won using that strategy
    wins <- 0
    for(cardoor in cardoors) {
      # possible doors for Monty
      montydoor <- doors[-c(1, cardoor)]

      # if more than 1, choose one at random
      if(length(montydoor) > 1) {
        montydoor <- sample(montydoor, 1)
      }
      # switch to remaining door
      chosendoor <- doors[-c(1, montydoor)]
      if(chosendoor == cardoor) wins <- wins+1
    }
  }

  print(paste0("Monty Hall Problem simulated ", n, " times!"))
  print(paste0("Strategy: ", ifelse(switch, "Switching", "Never switching")))
  print(paste0("Wins: ", wins, "/", n, ", or ", round((wins/n)*100, 2), "%"))
}
