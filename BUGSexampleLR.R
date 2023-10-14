rm(list = ls())
library(rjags)
setwd("C:/Users/16617/Documents/Bayes/Rjags")

germinationData = read.csv('germination.csv')

#logistic regression example
lrgModel = "
model {
  for (i in 1:N) {
    r[i] ~ dbin(p[i], n[i])
    b[i] ~ dnorm(0, tau)
    logit(p[i]) <- alpha0 + alpha1 * x1[i] + alpha2 * x2[i]
    + alpha12 * x1[i] * x2[i] + b[i]
  }
  alpha0 ~ dnorm(0, 1.0E-6)
  alpha1 ~ dnorm(0, 1.0E-6)
  alpha2 ~ dnorm(0, 1.0E-6)
  alpha12 ~ dnorm(0, 1.0E-6)
  tau ~ dgamma(0.001, 0.001)
  sigma <- 1 / sqrt(tau)

}
"
# Define the data
data <- list(N=21, r = c(germinationData$r), 
                   n = c(germinationData$n),
                  x1 = c(rep(0, 11), rep(1,10)),
                  x2 = c(rep(0,5), rep(1,6), rep(0,5), rep(1,5)))
 

jagsModelLR <- jags.model(textConnection(lrgModel), data = data, 
                          n.chains = 4)
update(jagsModelLR, 5000)

samples <- coda.samples(jagsModelLR, variable.names = c("alpha0",
            "alpha1", "alpha2", "alpha12", "b[1]", "sigma"),
            n.iter = 5000)

print(summary(samples))
plot(samples)

# Extract the posterior samples
par(mfrow=c(1,1))   # mfcol=1)

posterior_samples <- as.matrix(samples)
post_samples_df = as.data.frame(posterior_samples)
hist(post_samples_df$alpha0, freq=F, main = 'posterior alpha0')
kdP = density(post_samples_df$alpha0)
lines(kdP, col='red', lty=2, lwd = 3)
legend('topright','posterior distribution alpha0', col='red', lwd=3, lty=2)

kdP = density(post_samples_df$b[1:5000])
plot(kdP, col='red', lty=2, lwd = 3, main = 'posterior b[1]')
hist(post_samples_df$b[1:5000], freq=F, add=T)
legend('topleft','posterior distribution b[1]', col='red', lwd=3, lty=2)


# Plot the credibility interval for alpha0
# plot the highest probability density or alpha0 

# modify the model to product the predictive posterior for alpha0

