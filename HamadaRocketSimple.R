# Load required libraries
library(rjags)

# Define the data
data <- list(
  success = 3,
  trials = 11,
  trials13 = 13
)

# Define the JAGS model
model <- "
model {
  # Prior for probability of success (Beta distribution)
  # mildly informative
  
  p ~ dbeta(2.7, 5.8)

  # Likelihood (binomial)
  success ~ dbin(p, trials)
  
  #predictive posterior for number of successes in 13 trials
  #uncerttainity in p incorporated in 
  n_success ~ dbin(p, trials13)
}
"

# Compile the model
jags_model <- jags.model(textConnection(model), data = data, n.chains = 4)

# Burn-in and run the MCMC chains
update(jags_model, 1000)
samples <- coda.samples(jags_model, variable.names = c("p", "n_success"), 
          n.iter = 5000)

print(summary(samples))
plot(samples)

# Extract the posterior samples
posterior_samples <- as.matrix(samples)
post_samples_df = as.data.frame((posterior_samples))
hist(post_samples_df$p, freq=F, main = 'posterior p')
kdP = density(post_samples_df$p)
lines(kdP, col='red', lty=2, lwd = 3)
legend('topright','posterior distribution p', col='red', lwd=3, lty=2)

hist(post_samples_df$n_success, main='posterior predictive successes\n13 trials')

# Plot the posterior distribution
