par(mfrow=c(2,2))

n <- 100

X <- rnorm(n, mean=5, sd=1)

theta.hat <- exp(mean(X))

SE <- exp(mean(X))/sqrt(n)

#Delta method

sprintf("Delta Method")
sprintf("Lower bound is %s", theta.hat - 1.96 * SE)
sprintf("Upper bound is %s", theta.hat + 1.96 * SE)

#Bootstraps

B <- 100000

# Non-parametric bootstrap
t.nonparam <- c()

for(i in 1:B){
  x <- sample(X, n, replace = TRUE)
  t.nonparam <- append(t.nonparam, exp(mean(x)))
}


sprintf("Non-parametric bootstrap")
sprintf("Lower bound is %s", theta.hat - 1.96 * sd(t.nonparam))
sprintf("Upper bound is %s", theta.hat + 1.96 * sd(t.nonparam))
sprintf("Standard Error is %s", sd(t.nonparam))


#Parametric bootstrap

t.param <- c()

for(i in 1:B){
  x <- rnorm(n, mean = mean(X), sd=1)
  t.param <- append(t.param, exp(mean(x)))
}
sprintf("Parametric bootstrap")
sprintf("Lower bound is %s", theta.hat - 1.96 * sd(t.param))
sprintf("Upper bound is %s", theta.hat + 1.96 * sd(t.param))
sprintf("Standard Error is %s", sd(t.param))

#Histograms and plots

hist(t.param, main="parametric bootstrap")
hist(t.nonparam, main="nonparametric bootstrap")
t.grid <- seq(90,220, length=1000)
plot(t.grid, dnorm(t.grid, mean = theta.hat, sd = SE), type = "l")

#True distribution
theta.true <- c()
for(i in 1:B){
  x <- rnorm(n, mean = 5, sd=1)
  theta.true <- append(theta.true, exp(mean(x)))
}
hist(theta.true, main = "True distribution")
mean(theta.true)
sd(theta.true)

#Zhetessov Nur
