#--------------------------------------------------------------------------------
#Create a data set mu=5 consisting of n=100 observations. 
#--------------------------------------------------------------------------------

n <- 100
mu <- 5
x <- rnorm(n, mean=mu, sd=1)

#--------------------------------------------------------------------------------
# Use the bootstrap method 
#--------------------------------------------------------------------------------
est_hat_theta <- exp(mean(x))

bar_theta <- c()
for(i in 1:100000){
  rand_sampling <- sample(x, size = n, replace = TRUE)
  star_theta = exp(mean(rand_sampling))
  
  bar_theta <- append(bar_theta, star_theta)
}


#--------------------------------------------------------------------------------
#(a) Get the SE and 95 percent confidence interval for Theta 
#--------------------------------------------------------------------------------
SE = sqrt(var(bar_theta))

alpha = 1 - (95/100)
z_score = abs(qnorm(alpha/2))

Upp_bound = est_hat_theta + z_score * SE
Low_bound = est_hat_theta - z_score * SE

c(Low_bound,Upp_bound)

#--------------------------------------------------------------------------------
#(b) Plot a histogram of the bootstrap replications. Compare this to the true sampling distribution
#--------------------------------------------------------------------------------
hist(bar_theta, breaks = 1000, col = "darkmagenta",freq = FALSE)
lines(x = density(x = bar_theta), col = "red", lwd = 3)

true_theta = exp(mu)
abline(v=true_theta,col="blue",lwd=2)

#Zhetessov Nur M.