# Plot Multiple Simulations for Individual Infectiousness Profiles
# Against Analytical (ODE) Solution

require(ggplot2)
require(reshape2)

plotmulti <- function(stochastic, deterministic, iterates, beta, gamma, N, I){ #deterministic,
  sims = c()
  groups = c()
  
  analytic = deterministic(beta, gamma, N, I)
  
  for (i in 1:iterates){
    sim = stochastic(beta,gamma,N,I)
    simwithna = c(sim, rep(NA, times = N - length(sim)))
    sims <- c(sims, simwithna)
    name = paste0("group_", i)
    group = rep(name, times = N)
    groups <- c(groups, group)
  }
  
  df1 = data.frame(pop = c(rep(1:N,times = iterates)),
                   sims,
                   groups)
  
  
  df2 = data.frame(pop = analytic[1:N,5], 
                   sims = analytic[1:N,1],
                   groups = c(rep("deterministic", times = N)))
  
  
  ggplot(df1, aes(x = sims, y = pop)) + geom_line(aes(color = groups)) + geom_line(data = df2) +xlab("Time (days)") + ylab("Infected (people)") + ggtitle("Cumulative Infections for Delta Profile")
  
}