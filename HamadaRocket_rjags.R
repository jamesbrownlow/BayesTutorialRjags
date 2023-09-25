library(rjags)
library(coda)

# Define your data
observed_successes <- 3  # Replace with the number of observed successes
total_trials <- 11       # Replace with the total number of trials

# Specify the number of iterations, burn-in, and thinning
n_iterations <- 10000
n_burnin <- 1000
n_thin <- 1

# Define the model
model_code <- "
model {
  # Likelihood: Binomial distribution
  successes ~ dbin(p, trials)
  
  # Prior: Uniform prior for binomial probability
  #p ~ dunif(0, 1) # vague prior
  #p ~ dbeta(0.5, 0.5) #Jeffreys prior
  #p ~ dbeta(2.7, 5.8) # informative prior
  x ~ dexp(0.1)
  #logit
  logit(p) <- x
  
  #probit
  #probit(p) <- x
  
  #loglogistic
  #p <- ilogit(x)
  
}
"

# Create data list
data_list <- list(
  successes = observed_successes,
  trials = total_trials
)

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

abline(v = c(lCred, uCred), col='orange', lwd=3)

HPDI = HPDinterval((posterior_samples), p = 0.9)
print(HPDI)
abline(v = c(HPDI[1], HPDI[2]), col='red', lty=2, lwd=3)
legend('topright',c('90% credibility interval', '90% highest prob interval'), 
        col=c('orange','red'), lty = c(1,2), lwd=c(3,3))

grid()
