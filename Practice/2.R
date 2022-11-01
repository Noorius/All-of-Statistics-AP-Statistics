n <- 25
xbar <- 300
s <- 18.5
margin <-qt(0.975,df=n-1)*s/sqrt(n)
low <-xbar - margin
high <-xbar + margin
