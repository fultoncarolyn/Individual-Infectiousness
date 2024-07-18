# Cloud for Individual Infectiousness Profiles

require(ggplot2)
require(reshape2)

beta = 1.75
gamma = 0.35
N = 1000
I = 2

sim1 <- simulation(beta,gamma,N,I)
sim2 <- simulation(beta,gamma,N,I)
sim3 <- simulation(beta,gamma,N,I)
sim4 <- simulation(beta,gamma,N,I)

df1 = data.frame(pop = c(rep(1:N,times = 4)),
                newsim1 = c(sim1, rep(NA, times = N - length(sim1))),
                newsim2 = c(sim2, rep(NA, times = N - length(sim2))),
                newsim3 = c(sim3, rep(NA, times = N - length(sim3))),
                newsim4 = c(sim4, rep(NA, times = N - length(sim4))))
df1 <- melt(df1 ,  id.vars = 'pop', variable.name = 'series')

df2 = data.frame(pop = sir_model_solve[,5],
                 analytic = sir_model_solve[,1])
df2<- melt(df2 ,  id.vars = 'pop', variable.name = 'series')

p <- ggplot(df1, aes(value,pop)) + geom_line(aes(color = series)) + geom_line(data = df2)
p + ggtitle("Cumulative Infections for Delta Profile") + xlab("Time") + ylab("Number of Infected Individuals")
