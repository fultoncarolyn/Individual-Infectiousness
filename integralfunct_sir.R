library(deSolve)

# Define model
renewal_model <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), { #trying to understand structure of this line and reading in variables
    func <- function(x) {- dS(time - x) * exp(- alpha * x)}
    funcI <- integrate(func, lower = 0, upper = Inf)
    dS <- - beta * S * funcI
    dI <-  - dS + alpha * - funcI
    dR <-  alpha * funcI
    return(list(c(dS, dI, dR)))
  })
}

# Define params
parameters_values <- c(
  beta  = 0.007, # infectious contact rate (/person/day)
  gamma = 0.05    # recovery rate (/day)
)


# Define initial conditions
initial_values <- c(
  S = 999,  # number of susceptibles at time = 0
  I =   1,  # number of infectious at time = 0
  R =   0   # number of recovered (and immune) at time = 0
)

# timeframe
time_values <- seq(0, 10, by = 0.1)

# ODE Solver - ode with RK4
sir_model_solve <- ode(
  y = initial_values,
  times = time_values,
  func = renewal_model,
  parms = parameters_values,
  method = "rk4"
)

# ODE Solver - RK4

renewal_model_solve <- rk4(
  y = initial_values,
  times = time_values,
  func = renewal_model
)

sir_model_solve <- as.data.frame(sir_model_solve)


# Plotting with how do I force the ranges on axis (ex lost 1000)?
with(newsir_model_solve, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "number of people")
  # adding the time series of infectious:
  lines(time, I, col = "red")
  # adding the time series of recovered:
  lines(time, R, col = "green")
})

# adding a legend:
legend("right", c("susceptibles", "infectious", "recovered"), 
       col = c("blue", "red", "green"), lty = 1, bty = "n") 


