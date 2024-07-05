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

# Parameters
beta = 0.42 #use 0.175 for small tests
gamma = 0.35
N = 1000 

# Populations
I =   4  # number of infectious at time = 0
S = N - I


vone = integer(I)
vtwo = rexp(N,1/gamma) #infectiousness profile timing
vthree = integer(I)

unaccounted = c()
match = c()
comptau = c()
contacts = c()

##simulation <- function(vone, vtwo, vthree){
  #for (j in 1:N){
    for (i in 1:length(vthree)) #select index to apply to
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
        if (length(match) == 0){
          break
       }
        print(unaccounted)
        print(comptau)
        print(match)
        print(paste0("The k th individual to be evaluated infected at time:", vone[match],"for individual index:", match,"."))
  
        upsilon = rpois(1,beta/gamma) #number of contacts within the infectiousness period 
        print(paste0("Upsilon:", upsilon,"for iteration k and individual index:", match,"."))
        
        if (upsilon == 0){
          #no new contacts
          contacts <- NA
        } else {
          for (j in 1:upsilon){ #iterate over that vector to determine which contacts "stick"
            print(paste0("The population ratio:", S/N,"."))
            mu[j] <- rbinom(1,1,S/N)
            print(paste0("The probability of transmission:", mu[j], "for contact number:", j,"from individual index:", match,"."))
            if (mu[j] == 0){
              #no new contacts
              contacts[j] <- NA
            } else {
              contacts[j] <- vone[match] + vtwo[match]
              S <- S - 1
              print(paste0("The remaining susceptible population", S, "."))
            }
          }
        }

    contacts <- contacts[!is.na(contacts)] #remove any NA (not successful) contacts
    #print(paste0("Contacts:", contacts,"for iteration:", i,"resulting from individual:", match,"."))
    addcontacts = length(contacts)
    vone <- c(vone,contacts)
    vthree[match] <- 1
    vthree <- c(vthree,integer(addcontacts))
    
    ##}

print(vone)
print(vthree)
