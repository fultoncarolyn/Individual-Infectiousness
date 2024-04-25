library(deSolve)

# redefine params
beta = 1.75
gamma = 0.35
N = 1000


# Define function of model (ODEs) - why lists? other methods?
sir_model <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), { #trying to understand structure of this line and reading in variables
    dS <- - (beta * I * S) / N
    dI <-  (beta * I * S) / N - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}

# Define params
parameters_values <- c(
  beta  = beta, # infectious contact rate (/person/day)
  gamma = gamma    # recovery rate (/day)
)


# Define init conds
initial_values <- c(
  S = 999,  # number of susceptibles at time = 0
  I =   1,  # number of infectious at time = 0
  R =   0   # number of recovered (and immune) at time = 0
)

# time structure using by = or length = for stepsize is there a best way to do this?
time_values <- seq(0, 10, by = 0.1)

sir_model_solve <- ode(
  y = initial_values,
  times = time_values,
  func = sir_model,
  parms = parameters_values 
)

sir_model_solve <- as.data.frame(sir_model_solve)

lambdA <- sir_model_solve$I * Beta

newsir_model_solve <- cbind(sir_model_solve,lambdA)


# Plotting with how do I force the ranges on axis (ex lost 1000)?
with(newsir_model_solve, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "number of people")
  # adding the time series of infectious:
  lines(time, I, type = "l", lty = 2, col = "red")
  # adding the time series of recovered:
  lines(time, R, type = "l", col = "green")
  # adding the time series of force of infection
  lines(time, lambdA, type = "l", lty = 2, col = "purple")
})

# adding a legend:
legend("right", c("susceptibles", "infectious", "recovered", "force of infection"), 
       col = c("blue", "red", "green", "purple"), lty = 1:2, bty = "n") 

