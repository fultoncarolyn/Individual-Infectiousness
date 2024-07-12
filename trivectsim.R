# GOAL: Create vectors to simulate
# {infection times}
# {individual tau value} 
# {0 or 1 denote accounted for}

# 1. Identify which values are unaccounted for (0) from V3
# 2. Select smallest value from V1 from that subset
#     if == then select smallest in V2 of the two
#     if == then randomly select?
# 3. Apply upsilon to generate new infections
# 4. Denote 1 on previous iteration index in V3
# 5. Tracking using iterates to indicate the individual index it came from (work in progress)

simulation <- function(beta, gamma, N, I){ #iterates only for debugging
  #population requirements
  S = N - I
  
  #define vectors
  vone = integer(I) #initial infected
  vtwo = rexp(N,1/gamma) #infectiousness profile timing
  inputthree = integer(I) #accounting vector
  vfour = integer(I) #tracking vector
  
  for (k in 1:N){
    if (k == 1){ # probably a way to eliminate this 
      vthree = inputthree
    } else {
      vthree = vthree
    }
    unaccounted = c()
    match = c()
    comptau = c()
    contacts = c()
    mu = c()
    
    #select index to apply to next
    for (i in 1:length(vthree)){ 
      if (vthree[i] == 0){
        unaccounted[i] <- vone[i]
      } else {
        unaccounted[i] <- NA
      }
        match <- which(unaccounted == min(unaccounted, na.rm=T))
      
        if (length(match) > 1){
          for (i in match)
            comptau[i] <- vtwo[i]
            compmatch <- which(comptau == min(comptau, na.rm=T))
            if (length(compmatch) > 1){
              match <- sample(match,1)
              print(paste0("The next individual was selected randomly due to matching tau values."))
            } else {
              match <- compmatch
            }
        }
    }
    if (length(match) == 0){ #everyone has been accounted for
      break
    }
    if (length(vone) > N){ #exceeding population limit (stop())
      break
    }
        print(paste0("The ", k, "th individual to be evaluated infected at time: ", vone[match]," for individual index: ", match,"."))
  
        # begin the infection regime
        upsilon = rpois(1,beta/gamma) #number of contacts within the infectiousness period 
        print(paste0("Upsilon: ", upsilon," for iteration ", k, " from individual index: ", match,"."))
        
        if (upsilon == 0){
          #no new contacts
          contacts <- NA
        } else {
          for (j in 1:upsilon){ #iterate over that vector to determine which contacts "stick"
            mu[j] <- rbinom(1,1,S/N)
            print(paste0("The probability of transmission: ", mu[j], " for contact number: ", j," from individual index: ", match,"."))
            if (mu[j] == 0){
              #no new contacts
              contacts[j] <- NA
            } else {
              contacts[j] <- vone[match] + vtwo[match]
              S <- S - 1
              print(paste0("The remaining susceptible population ", S, "."))
            }
          }
        }

    contacts <- contacts[!is.na(contacts)] #remove any NA (not successful) contacts
    print(paste0("Contacts: ", contacts," for iteration k resulting from individual: ", match,"."))
    
    addcontacts = length(contacts)
    vone <- c(vone,contacts)
    vthree[match] <- 1
    vthree <- c(vthree,integer(addcontacts))
    keeptrack = rep(match, times = addcontacts)
    vfour <- c(vfour,keeptrack)
    
  print(vone)
  print(vthree)
  print(vfour)
    }
    print(paste0("Simulation ended on iteration ", k, " where there were ", length(vone), " individuals accounted for."))
}
