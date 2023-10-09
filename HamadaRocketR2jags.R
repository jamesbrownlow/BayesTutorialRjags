library(rjags)
library(R2jags)

setwd('C:/Users/16617/Documents/Bayes/Rjags')

# Define your data
observed_successes <- 3  # Replace with the number of observed successes
total_trials <- 11       # Replace with the total number of trials

# Specify the number of iterations, burn-in, and thinning
n_iterations <- 10000
n_burnin <- 1000
n_thin <- 2

# Define the model
model_code <- "
model {
  # Likelihood: Binomial distribution
  successes ~ dbin(p, trials)
  
  # Prior: Uniform prior for binomial probability
  p ~ dunif(0, 1) # vague prior
  #p ~ dbeta(0.5, 0.5) #Jeffreys prior
  #p ~ dbeta(2.7, 5.8) # informative prior
}
"

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

fileNameData=list(x=c(rep(1,3), rep(0,8)),n=11)
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






# Initialize JAGS model
jags_model <- jags.model(textConnection(model_code), data = data_list, n.chains = 3)

# Run the MCMC simulation
jags_samples <- coda.samples(jags_model, variable.names = "p", 
                             n.iter = n_iterations, n.burnin = n_burnin, thin = n_thin)

# Load the coda package for posterior analysis
library(coda)

# Extract the posterior samples
posterior_samples <- as.mcmc(jags_samples[[1]])

# Summary statistics of the posterior distribution
summary(posterior_samples)

# Plot the posterior distribution
plot(posterior_samples, main = "Posterior Distribution of p")

kd = density(posterior_samples)
plot(kd, lwd=3)

sortedSamples = sort(posterior_samples)
nSorted = length(sortedSamples)
lCred = sortedSamples[round(nSorted*0.05)]
uCred = sortedSamples[round(nSorted*0.95)]
print(paste('90% credibility interval: (',round(lCred,2),',',round(uCred,2),')'))

abline(v = c(lCred, uCred), col='orange')
