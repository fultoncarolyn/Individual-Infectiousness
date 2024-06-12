# Define SIR Model

# Parameters
beta = 1.75
gamma = 0.35
N = 1000 
#gen -> number of generations to loop over

# Populations
I =   3  # number of infectious at time = 0
#R0 = beta/gamma

# Create vector of initial infected
init = integer(I);
 
func1 <- function(vector,gen){
  
 #infectdata <- matrix(NA, nrow = iter, ncol = I)
 #for (n in 1:iter){
 #  
 #}
  infectdata = c(vector)
  for (j in 1:gen){
    if (j == 1){
      nextgen = vector
    } else {
      nextgen = prevgen
    }
      for (i in 1:length(nextgen)){
        ##nextgen[i] <- nextgen[i] + rexp(1, 1/gamma) #timing of new infection
        tau = rexp(1,1/gamma) #infectiousness profile timing
        upsilon = rpois(1,beta/gamma) #number of contacts within the infectiousness period
        #sprintf("Value of tau: %d", tau)
        #sprintf("Value of upsilon: %d", upsilon)
        container = c()
        nextgen[i] <- nextgen[i] + tau
        container[i] <- c(nextgen[i],rep(nextgen[i],times = (upsilon-1))) #this is probably overwriting i
        #multicontact <- c(nextgen[i],rep(nextgen[i],times = (upsilon-1)))
        #mu[i] <- rdist(S/N) #successful transmission of infection
        output <- sort(container) # I think I am losing where the infection came from, do we care?
        prevgen = output
      }
  infectdata = c(infectdata,prevgen)
  #return(infectdata)
  }
  sortinfectdata <- sort(infectdata)
  return(sortinfectdata)
  }
