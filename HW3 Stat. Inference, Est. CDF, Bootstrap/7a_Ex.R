n<-50
x <- runif(n, 0, 1)

hat_theta = max(x)

bar_theta <- c()
for(i in 1:100000){
  rand_sampling <- sample(x, size = n, replace = TRUE)
  star_theta = max(rand_sampling)
  
  bar_theta <- append(bar_theta, star_theta)
}

SE = sqrt(var(bar_theta))

alpha = 1 - 0.95
z = abs(qnorm(alpha/2))

Lower_bound = hat_theta - z * SE
Upper_bound = hat_theta + z * SE

Lower_bound
Upper_bound

hist(bar_theta, breaks = 100, col = "darkmagenta",freq = FALSE, xlim=c(0.8, 1))
#lines(x = density(x = bar_theta), col = "red", lwd = 3)

true_theta = max(1)
abline(v=true_theta,col="blue",lwd=2)

#Zhetessov Nur M.
