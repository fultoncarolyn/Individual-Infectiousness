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
I =   3  # number of infectious at time = 0
S = N - I


vone = integer(I)
vtwo = rexp(N,1/gamma) #infectiousness profile timing
vthree = c(0,1,0)

unaccounted = c()
match = c()
comptau = c()
contacts = c()

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
      match <- which(comptau == min(comptau, na.rm=T))
      #add random if tau == tau
    }
  

upsilon = rpois(1,beta/gamma) #number of contacts within the infectiousness period 
mu = rbinom(upsilon,1,S/N) #probability of transmission for each contact

if (length(mu) == 0){
  #no new contacts
  contacts <- NULL
} else {
  for (j in 1:length(mu)){ #iterate over that vector to determine which contacts "stick"
    #successful contact
    if (mu[j] == 1){
      contacts[j] <- vone[match] + vtwo[match]
      S <- S - 1
    #failed contact
    } else {
      contacts[j] <- NULL
    }
  }
}
contacts <- contacts[!is.na(contacts)] #remove any NA (not successful) contacts
addcontacts = length(contacts)
vone <- c(vone,contacts)
vthree[match] <- 1
vthree <- c(vthree,integer(addcontacts))

print(unaccounted)
print(comptau)
print(match)
print(upsilon)
print(mu)
print(contacts)
print(vone)
print(vthree)
