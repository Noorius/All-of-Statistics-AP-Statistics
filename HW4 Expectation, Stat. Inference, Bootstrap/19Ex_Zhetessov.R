curve(dunif(x, min = 0, max = 1), 
      from = -0.5, to = 1.5, 
      n = 100000, 
      col = "blue", 
      lwd = 2,
      ylab = 'Probability')

#-------------------------------------------------------------------#

inf <- 10000
xbar <- rep(0,inf)

n1 <- 1
n5 <- 5
n25 <- 25
n100 <- 100

for(i in 1:inf){
  x <- runif(n100, min=0, max=1)
  xbar[i] = mean(x)
}

hist(xbar, main="Sampling distribution of the mean, n = 100", )
E <- mean(xbar)
Var <- var(xbar)

#------------------------------------------------------------------#

Expected = c();
Variance = c();

for(i in 1:1000){
  samp = runif(i, min=0, max=1)
  
  Expected <- append(Expected, mean(samp))
  Variance <- append(Variance, abs(var(samp)-(1/12)))
}

nn <- seq(1,1000)

plot(nn, Expected, type='l', ylim = c(0.25,0.8), col="darkblue", main="Expectations", xlab="n", ylab="E(X)")
abline(h=0.5, col='red')

plot(Variance, type='l', col="darkblue", main="Variances", xlab="n", ylab="Difference", ylim=c(0,0.08))
lines(nn, 1/(12*nn), type='l')

#Zhetessov Nur