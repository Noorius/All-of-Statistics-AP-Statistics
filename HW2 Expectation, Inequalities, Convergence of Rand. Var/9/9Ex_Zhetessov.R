
t <- c()
x <- rnorm(10001, mean=0, sd=1)
for(val in 1:10001){
  t <- append(t, mean(x[1:val]))
}
plot(1:10001, t, ylim = c(-0.05,0.5))



x_cauchy <- rcauchy(10001)
t1 <- c()
for(val in 1:10001){
  t1 <- append(t1, mean(x_cauchy[1:val]))
}
d <- seq(1,10001)
plot(d, t1)


