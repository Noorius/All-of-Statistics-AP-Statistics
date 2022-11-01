set.seed(0)

n <- 20
mu <-5
sigma <- 3

samples <- replicate(rnorm(n,mu, sigma), n=100)

sample_means <- apply(samples, 2, mean)
sample_var <- apply(samples, 2, function(x){
  return ((1/n)*sum(x^2)-sum(x/n)^2)
})

mean(sample_means); mean(sample_var); sqrt(mean(sample_var));


#--------------------------------#


set.seed(0)
x<-rgamma(1000, 5, 7)

mu_1 <- mean(x)
mu_2 <- mean(x^2)

mu_1^2 / (mu_2 - mu_1 ^ 2)

mu_1 / (mu_2 - mu_1 ^ 2)

