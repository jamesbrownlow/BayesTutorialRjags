
rm(list=ls())
library(rjags)
library(coda)
library(fitdistrplus)

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
  p ~ dbeta(2.7, 5.8) # informative prior
  #x ~ dexp(0.1)
  
  #logit   p = x/(x+1)  same as p <- ilogit(x)
  #logit(p) <- x    
  
  #probit  p = inverse normal CDF(x)
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

#initialize p with a function
initialP = function(){
  list(p=runif(1))
}

# Initialize JAGS model
jags_model <- jags.model(textConnection(model_code), data = data_list,
                    n.chains = 4, inits = initialP)

# Run the MCMC simulation
jags_samples <- coda.samples(jags_model, variable.names = "p", 
                    n.iter = n_iterations, n.burnin = n_burnin, thin = n_thin)

summary(jags_samples)

#gelman.plot((jags_samples))
#gelman.diag(jags_samples)

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

posterior.p = as.vector(posterior_samples, mode = 'numeric')
meanPost = mean(posterior.p)
modePost = max(kd$y)
modeProb = kd$x[kd$y==modePost]
stdPost = sd(posterior.p)
pAxis = seq(0, 0.8, length.out=50)
lines(pAxis, dnorm(pAxis, mean=meanPost, sd = stdPost), col='green', lty=2,lwd=3)

sortedSamples = sort(posterior_samples)
nSorted = length(sortedSamples)
lCred = sortedSamples[round(nSorted*0.05)]
uCred = sortedSamples[round(nSorted*0.95)]
print(paste('90% credibility interval: (',round(lCred,2),',',round(uCred,2),')'))

abline(v = c(lCred, uCred), col='orange', lwd=3)

HPDI = HPDinterval((posterior_samples), p = 0.9)
print(HPDI)
abline(v = c(HPDI[1], HPDI[2]), col='red', lty=2, lwd=3)
legend('topright',c('90% credibility interval', '90% highest prob interval',
                    'posterior density','posterior approximation'), 
        col=c('orange','red','black','green'), lty = c(1,2,1,2), 
       lwd=c(3,3,3,3))

grid()

# A posterior predictive distribution accounts for uncertainty about 
# p, taken from the posterior distribution of p.  

# Posterior predictive distribution is original likelihood times a 'prior'
# that is the posterior distribution.
#
# It's a predictions on 'more' data, given your current data. 

# To find the posterior predictive distribution, group multiply the
# likelihood of data by prior. 
# Posterior of Binomial with Beta Prior

# predictive probability of a x successes is
# p(x|data) = integral(likelihood(x|p)*posterior(p|data))

# we do not have a closed form for posterior(p|data) thus
# approximate it with a 'fitted' distribution or use data product.


postPredModel_code <- "
model {
  # Likelihood: Binomial distribution
  successes ~ dbin(p, trials)
  
  # Prior: normal approximation to posterior
  p ~ dnorm(m, 1/s*s) # the new prior for predicted posterior
}
"
# data list
postPredModel_list = list(successes= observed_successes, m = meanPost, 
                          trials = total_trials, s = stdPost)

# initialize  the model
jags_model_Post <- jags.model(textConnection(postPredModel_code), 
                      data = postPredModel_list, n.chains = 4,
                      inits = initialP)

# Run the MCMC simulation
jags_samples_Post <- coda.samples(jags_model_Post, variable.names = "p", 
                  n.iter = n_iterations, n.burnin = n_burnin, thin = n_thin)

summary(jags_samples_Post)
plot(jags_samples_Post)

# Extract the posterior samples
post_Pred_samples <- as.mcmc(jags_samples_Post[[1]])

kd_PostP = density(post_Pred_samples)

# fit a beta distribution to the post_Pred_samples
# need this for the MCMC to get posterior predictive successes
bd = as.numeric(post_Pred_samples)
fit_beta = fitdist(bd, 'beta')
print(fit_beta)


plot(kd, lwd=3, col='red', lty=2)
lines(kd_PostP, lwd=3, xlab='probability of success', col='black')

HPDI = HPDinterval((jags_samples_Post), p = 0.9)
print(HPDI)
print(as.numeric(HPDI[[1]][1:2]))
abline(v = c(HPDI[[1]][1], HPDI[[1]][2]), col='orange', lty=2, lwd=3)

legend('topright',c('posterior predictive','posterior', 'post pred HPDI 90%'), 
       col=c('black', 'red','orange'),
       lty=c(1,2,2), lwd = c(3,3,3))
grid()

rHat = gelman.diag(jags_samples_Post)
print(str(rHat))

#####################################################################
## posterior predictive on number of failures in next 13 launches

newTrials = 13

postPredModelSuccess_code <- "
model {
   # likelihood
   successes ~ dbin(p, trials)

   # prior
   p ~ dbeta(s1, s2)
  
}
"

# Create data list for prdictive posterior, number of successes
#postPredSuccesses_list = list(trials = newTrials)

postPredSuccesses_list <- list(s1 = as.numeric(fit_beta[[1]]['shape1']),
                               s2 = as.numeric(fit_beta[[1]]['shape2']),
                               trials = newTrials
)

# data list
initialSucc = function(){ 
              p = rnorm(1, m = 0.3, s = 0.1) # approximate
              list(successes = rbinom(1,newTrials,p))
}


# initialize  the model
jags_model_PostSucc <- jags.model(textConnection(postPredModelSuccess_code), 
                              data = postPredSuccesses_list, n.chains = 4, 
                              inits = initialSucc)

# Run the MCMC simulation
jags_samples_PostSucc <- coda.samples(jags_model_PostSucc, n.iter = n_iterations,
                      variable.names = "successes", n.burnin = n_burnin, 
                      thin = n_thin)

summary(jags_samples_PostSucc)
post_Pred_samplesSucc <- as.mcmc(jags_samples_PostSucc[[1]])
hist(post_Pred_samplesSucc, freq = F, main='predictive posterior successes, 13 trials')


