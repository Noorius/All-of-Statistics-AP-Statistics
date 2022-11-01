LSAT = c(576, 635, 558, 578, 666, 580, 555, 661, 651, 605, 653, 575, 545, 572, 594)
GPA = c(3.39, 3.30, 2.81, 3.03, 3.44, 3.07, 3.00, 3.43, 3.36, 3.13, 3.12, 2.74, 2.76, 2.88, 3.96)

plot(LSAT,GPA, type = 'p')

LSAT_mean = mean(LSAT)
GPA_mean = mean(GPA)

theta = sum((LSAT-LSAT_mean)*(GPA-GPA_mean)) / sqrt(sum((LSAT-LSAT_mean)*(LSAT-LSAT_mean))*sum((GPA-GPA_mean)*(GPA-GPA_mean)))

theta

#--------------------------------------------------------#

theta_hat = c()

for(i in 1:1000000){
  boot1 = sample(LSAT, 15, replace = TRUE)
  boot2 = sample(GPA, 15, replace = TRUE)
  
  mean1 = mean(boot1)
  mean2 = mean(boot2)
  
  theta_hat[i] = sum((boot1-mean1)*(boot2-mean2)) / sqrt(sum((boot1-mean1)*(boot1-mean1))*sum((boot2-mean2)*(boot2-mean2)))
}

SE = sd(theta_hat)

SE

#--------------------------------------------------------#

normal = c(theta - 1.96 * SE, theta + 1.96 * SE)
percentile = c(quantile(theta_hat,0.025), quantile(theta_hat,0.975))
pivotal = c(2*theta - quantile(theta_hat,0.975), 2*theta - quantile(theta_hat,0.025))

theta
SE
normal
percentile
pivotal
#Zhetessov