# Parameters
beta = 0.21 #use 0.175 for small tests
gamma = 0.35
N = 1000 

# Populations
I =   2  # number of infectious at time = 0
S = N - I

# Initial Conditions
input = integer(I)
tracking = integer(I)

funct <- function(vector){
  size = length(vector)
    for (i in 1:N){
      tau = rexp(1,1/gamma) #infectiousness profile timing
      print(paste0("New tau:", tau,"for individual:", i,"."))
      
      upsilon = rpois(1,beta/gamma) #number of contacts within the infectiousness period 
      print(paste0("New upsilon:", upsilon,"for individual:", i,"."))
      
      contacts = c() #create empty vector to input successful infections and their timing
      
      if (upsilon == 0){ #case of NO NEW contacts
        contacts <- NA
        print(paste0("No new contacts from individual:", i,"."))
        
      } else { # case of AT LEAST 1 NEW contact
        mu = rbinom(upsilon,1,S/N) #probability of transmission for each contact
        
        for (j in 1:length(mu)){ #iterate over that vector to determine which contacts "stick"
          
          #successful contact
          if (mu[j] == 1){
            print(paste0("The probability of transmission:", mu[j], "for contact:", j,"from individual:", i,"."))
            contacts[j] <- vector[i] + tau
            S <- S - 1
            print(paste0("The remaining susceptibles:", S, "following contact:", j,"from individual:", i,"."))
            
          #failed contact
          } else {
            print(paste0("The probability of transmission:", mu[j], "(failed) for contact:", j,"from individual:", i,"."))
            contacts[j] <- NA
          }
      }
      
      }
      contacts <- contacts[!is.na(contacts)] #remove any NA (not successful) contacts
      #contacts <- sort(contacts) #this might not be necessary since they get ordered a few steps later anyway...
      addsize = length(contacts) #measure how many new contacts
      
      # Tracking - append number of new contacts for ith iteration...
      newtracks = rep(i,times=addsize)
      tracking <- c(tracking,newtracks)
      
      #... to the vector of contacts
      vector <- c(vector,contacts)
      
      # Combine and order based on time of infection/order of individual
      combined <- cbind(vector,tracking)
      orderedbytime <- combined[order(vector),]
      orderedbyindiv <- combined[order(tracking),]
      
      # Select just the tracking to loop to the next iteration
      tracking <- orderedbytime[,"tracking"]
      
      # Select just the cumulative infection times
      output <- orderedbytime[,"vector"]
      
      # Total infected at the end of epidemic
      totalI = length(output)
      #print(paste0("New I pop:", newI,"for iteration:", i,".")) #Infectious/Infected population tracking
      
      # Remaining susceptibles
      Sleft = N - totalI
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
  
  # Plotting 
  infecttime = output 
  infectppl = 1:length(output)
  
  plot(infecttime,infectppl,type="l")
  
}
