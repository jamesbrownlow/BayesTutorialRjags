rm(list=ls())

library(R2jags)

#set local directory
setwd('C:/Users/16617/Documents/Bayes/Rjags')

# Define your data
observed_successes <- 3  # Replace with the number of observed successes
total_trials <- 11       # Replace with the total number of trials


## from the slides...
cat(" # model is a character string
model {
 for(i in 1:n) {
 x[i] ~ dbern(theta)
 }
 theta ~ dbeta(1,1) #prior on theta
 }" , # end of BUGS model
file='fileName.txt') # end of cat()

# Create data list
data_list <- list(
  successes = observed_successes,
  trials = total_trials
)

fileNameData=list(x=c(rep(1,3), rep(0,8)),n=11) #uses observed successes/fails
fileNameInits = function() {list(theta = rbeta(1,1,1))}
fileNameParms = c("theta")

fileNameJags=jags(
 data = fileNameData,
 inits = fileNameInits,
 parameters.to.save = fileNameParms,
 model.file = "fileName.txt",
 n.iter = 2000,
 n.thin = 1,
 n.burnin = 500,
 n.chains = 4,
 DIC = TRUE)


fileNameJagsMC2 = autojags(fileNameJags)
attach.jags(fileNameJagsMC2)
plot(density(theta))


