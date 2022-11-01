n <- 200
X1 <- 160
X2 <- 148

p1.hat = X1 / n
p2.hat = X2 / n

#Parametric bootstrap method 

B <- 10000
tau.boot <- c()

for(i in 1:B){
  x1 <- rbinom(1,n,p1.hat)
  x2 <- rbinom(1,n,p2.hat)
  tau.boot <- append(tau.boot, (x1/n)-(x2/n) )
}

#Phi and phi.hat has been found

phi.hat = p1.hat - p2.hat

phi.hat         # 0.06000 
mean(tau.boot)  # 0.06062

#--------------------------------------------------------------#
#90% CI

sprintf("Lower bound is %s", phi.hat - 1.645 * sd(tau.boot))
sprintf("Upper bound is %s", phi.hat + 1.645 * sd(tau.boot))

#Zhetessov Nur
