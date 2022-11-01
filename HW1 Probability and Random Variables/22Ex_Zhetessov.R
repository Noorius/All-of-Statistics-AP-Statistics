p <- 0.3

x1 <- rbinom(20000, 10, p)
x2 <- rbinom(20000, 100, p)
x3 <- rbinom(20000, 1000, p)

print(paste("X1 mean:",mean(x1)))
print(paste("X1 np:",p*10))

print(paste("X2 mean:",mean(x2)))
print(paste("X2 np:",p*100))

print(paste("X3 mean:",mean(x3)))
print(paste("X3 np:",p*1000))


hist(x1)
abline(v = mean(x1), col="red", lwd=1)
abline(v = p*10, col="blue", lwd=1)
text(x = 8, y = 2000, paste("Mean =", mean(x1)), col = "red", cex = 2)
text(x = 8, y = 1500, paste("n*p =", 10*p), col = "blue", cex = 2)

hist(x2, breaks=50)
abline(v = mean(x2), col="red", lwd=1)
abline(v = p*100, col="blue", lwd=1)
text(x = 45, y = 1500, paste("Mean =", mean(x2)), col = "red", cex = 2)
text(x = 45, y = 1200, paste("n*p =", 100*p), col = "blue", cex = 2)

hist(x3, breaks=100)
abline(v = mean(x3), col="red", lwd=1)
abline(v = p*1000, col="blue", lwd=1)
text(x = 350, y = 500, paste("Mean =", mean(x3)), col = "red", cex = 2)
text(x = 350, y = 400, paste("n*p =", 1000*p), col = "blue", cex = 2)

#Zhetessov Nur M.
#Advanced Statistics
