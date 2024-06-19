# Parameters
beta = 1.75
gamma = 0.35
N = 1000 

# Populations
I =   2  # number of infectious at time = 0

input = integer(I)

funct <- function(vector){
  size = length(vector)
  #while (i <= size) {
    for (i in 1:N){
      tau = rexp(1,1/gamma) #infectiousness profile timing
      #print(paste0("New tau:", tau,"for iteration:", i,"."))
      upsilon = rpois(1,beta/gamma) #number of contacts within the infectiousness period
      #print(paste0("New upsilon:", upsilon,"for iteration:", i,"."))
      ##mu = rbern(upsilon,S/N) #pribability of transmission (Need to talk through implementing this...)
      contacts = c()
      contacts <- rep(vector[i]+tau,times = (upsilon))
      addsize = length(contacts)
      vector <- c(vector,contacts)
      newI = length(vector)
      #print(paste0("New I pop:", newI,"for iteration:", i,".")) #Infectious/Infected population tracking
      newS = N - newI
      #print(paste0("New S pop:", newS,"for iteration:", i,"."))#Susceptible population tracking
      output <- sort(vector)
      #print(output)
      
      size <- size + addsize
      print(paste0("New size:", size,"for iteration:", i,"."))
      if (size > N)
        break
      if (i >= size)
        break
    }
  #print(output)
  infecttime = output #what's our timescale?
  infectppl = 1:length(output)
  
  plot(infecttime,infectppl)
  
  #with(infecteddata, {
    #plot(infecteddata, y)
    #}
}
