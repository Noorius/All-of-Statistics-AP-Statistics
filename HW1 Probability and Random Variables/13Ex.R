n <- 10000

x <- rnorm(n)

y <- exp(x)

hist(y,breaks = 1000, xlim=c(0,10), prob=TRUE) #Draw a histogram

lines(density(y), col="red", lwd=2) #Compare with the Probability Density Function

#Zhetessov Nur