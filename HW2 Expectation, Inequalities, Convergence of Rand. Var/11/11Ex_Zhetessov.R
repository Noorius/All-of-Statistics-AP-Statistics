x <- sample(c(1,-1), size=10001, prob = c(0.5,0.5), replace = TRUE)
y <- cumsum(x)

d <- seq(1,10001)

plot(d, y)

#zhetessov Nur

