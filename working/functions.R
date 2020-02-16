### Function to solve for per capita growth rate of a population

dNNdt <- function(N1, N2, years = 1) {
  dNNdt <- ((N2-N1)/years)/N1
  return(dNNdt)
}

dNNdt(50, 100, years = 2)

population <- function(N1, dNNdt, years = 1) {
  N2 <- N1+(N1*dNNdt)*years
  return(N2)
}

population(50, 1)
population(50, 1, 2)
