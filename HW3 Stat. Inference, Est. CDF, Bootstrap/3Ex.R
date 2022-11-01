#----------------------------------------------------------------------------------------
#Generate 100 observations from a N(O,l) distribution.
#----------------------------------------------------------------------------------------
par(mfrow=c(2,2))
grid <- seq(-3,3, length=1000)
Fn <- pnorm(grid)
plot(grid, Fn, type="l", xlab="X value",ylab="CDF", sub="Theoretical CDF")


n <- 100
x <- rnorm(n, mean = 0, sd = 1)
x <- sort(x)

Fn_hat <- (1:n)/n #ecdf(x) 
plot(x, Fn_hat, type="s", xlab="X Value",ylab="CDF", sub="Empirical CDF", xlim=c(-3,3))

plot(grid, Fn, type="l",xlab="X Value",ylab="CDF")
lines(x,Fn_hat,type="s", lty=3,col=4,lwd=3)

#----------------------------------------------------------------------------------------
#Compute a 95 percent confidence band for the CDF F
#----------------------------------------------------------------------------------------
alph <- 1 - 0.95 
epsilon <- sqrt(1 / (2 * n) * log(2 / alph)) # I did by the formulas on the page 99 in Wasserman's Book

L <- pmax(Fn_hat - epsilon, 0)
U <- pmin(Fn_hat + epsilon, 1)

plot(grid, Fn, type="l", xlab="x", ylab="cdf", xlim=c(-3,3))

h <- ecdf(x)
plot(h, xlim=c(-3,3))
lines(x,L,lty=3,col=2,type="s")
lines(x,U,lty=3,col=2,type="s")

#----------------------------------------------------------------------------------------
#Repeat this 1000 times and see how often 
#the confidence band contains the true distribution function.
#----------------------------------------------------------------------------------------
ans <- c()

for(i in 1:1000){
  grid <- seq(-3,3, length=1000)
  Fn <- pnorm(grid)
  
  n <- 100
  x <- rnorm(n, mean = 0, sd = 1)
  x <- sort(x)
  
  Fn_hat <- (1:n)/n #ecdf(x) 
  
  
  alph <- 1 - 0.95 
  epsilon <- sqrt(1 / (2 * n) * log(2 / alph))
  
  L <- pmax(Fn_hat - epsilon, 0)
  U <- pmin(Fn_hat + epsilon, 1)
  
  fraction = c()
  for (i in 1:100)
  {
    logic <- U[i]>=pnorm(x[i]) && L[i]<=pnorm(x[i])
    fraction <- append(fraction, logic)
  }
  
  ans <- append(ans,all(fraction))
}

mean(ans) # About 0.955 which is >=0.95 (95% confidence interval)

#----------------------------------------------------------------------------------------
#Repeat using data from a Cauchy distribution.
#----------------------------------------------------------------------------------------
ans <- c()

for(i in 1:1000){
  grid <- seq(-3,3, length=1000)
  Fn <- pnorm(grid)
  
  n <- 100
  
  x <- rcauchy(n) #I only changed the function for random sample
  x <- sort(x)
  
  Fn_hat <- (1:n)/n #ecdf(x) 
  
  
  alph <- 1 - 0.95 
  epsilon <- sqrt(1 / (2 * n) * log(2 / alph))
  
  L <- pmax(Fn_hat - epsilon, 0)
  U <- pmin(Fn_hat + epsilon, 1)
  
  fraction = c()
  for (i in 1:100)
  {
    logic <- U[i]>=pnorm(x[i]) && L[i]<=pnorm(x[i])
    fraction <- append(fraction, logic)
  }
  
  ans <- append(ans,all(fraction))
}

mean(ans) # About 0.19 - 0.20 which is (< 0.95) lower than 95% confidence interval

#Zhetessov Nur M.
