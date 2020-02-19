### Function to solve for per capita growth rate of a population
# N1 = population at time 1
# N2 = population at time 2

dNNdt <- function(N1, N2, years = 1) {
  N1 = ifelse((N1 < 0), return("population must be greater than zero"), N1)
  N2 = ifelse((N2 < 0), NA, N2)
  dNNdt <- ((N2-N1)/years)/N1
  return(dNNdt)
}

# example 2:
# dNNdt(-50, 150)

# Example 2:
# pop1 <- c(30, 20, -40, 50)
# pop2 <- c(40, -50, 60, 80)
# dNNdt(pop1, pop2)

### Function to solve for popultion
population <- function(N1, dNNdt, years = 1) {
  N2 <- N1+(N1*dNNdt)*years
  return(N2)
}

population(50, 1)

