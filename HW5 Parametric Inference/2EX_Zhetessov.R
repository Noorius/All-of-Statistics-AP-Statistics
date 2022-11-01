a <- 1
b <- 3
n <- 10

rs <- runif(n, min=a, max=b)

m <- c()

for(i in 1:10000){
  s <- sample(rs, n, replace = TRUE)
  m <- append(m, (max(s)+min(s))/2 )
}

MSE <- var(m)

MSE

#Zhetessov Nur