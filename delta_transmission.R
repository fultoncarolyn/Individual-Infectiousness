# Parameters
beta = 1.75 #use 0.175 for small tests
gamma = 0.35
N = 1000 

# Populations
I =   2  # number of infectious at time = 0
S = N - I

input = integer(I)
tracking = integer(I)

funct <- function(vector){
  size = length(vector)
    for (i in 1:N){
      tau = rexp(1,1/gamma) #infectiousness profile timing
      print(paste0("New tau:", tau,"for individual:", i,"."))
      upsilon = rpois(1,beta/gamma) #number of contacts within the infectiousness period 
      #switch to binomial(N,Pinf=R0/N) impacts smaller pop size or infection num grows
      print(paste0("New upsilon:", upsilon,"for individual:", i,"."))
      contacts = c()
      if (upsilon == 0){
        contacts <- contacts
        print(paste0("No new contacts from individual:", i,"."))
      } else {
        for (j in 1:upsilon){
          mu = rbinom(upsilon,1,S/N) #probability of transmission
          print(paste0("The probability of transmission:", mu, "for contact:", j,"."))
          if (mu[j] == 1){
            contacts[j] <- vector[i] + tau
            contacts <- contacts[!is.na(contacts)]
            S <- S - 1
            ##print(paste0("The remaining susceptibles:", S, "following contact:", j,"."))
          } else {
            next
          }
      }
      
      }
      ##print(contacts)
      contacts <- sort(contacts)
      addsize = length(contacts)
      
      newtracks = rep(i,times=addsize)
      tracking <- c(tracking,newtracks)
      
      vector <- c(vector,contacts)
      
      orderbytime <- cbind(vector,tracking)
      orderedbytime <- orderbytime[order(vector),]
      
      tracking <- orderedbytime[,"tracking"]
      output <- orderedbytime[,"vector"]
      newI = length(vector)
      #print(paste0("New I pop:", newI,"for iteration:", i,".")) #Infectious/Infected population tracking
      newS = N - newI
      #print(paste0("New S pop:", newS,"for iteration:", i,"."))#Susceptible population tracking
      
      size <- size + addsize
      ##print(paste0("New size:", size,"for iteration:", i,"."))
      if (size >= N)
        break
      if (i >= size)
        break
    }
  print(output)
  print(tracking)
  infecttime = output 
  infectppl = 1:length(output)
  
  plot(infecttime,infectppl)
  
}
