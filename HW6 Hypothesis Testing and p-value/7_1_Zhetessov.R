#a) Wald Test
vec1 = c(.225, .262, .217, .240, .230, .229, .235, .217)
vec2 = c(.209, .205, .196, .210, .202, .207, .224, .223, .220, .201)

estimator =  mean(vec1)-mean(vec2)
SE = sqrt((var(vec1)/8)+(var(vec2)/10))

W = (estimator)/SE
W

CI= c(estimator - 1.96 * SE, estimator + 1.96 * SE)
CI

#b) Permutation Test
vec3 = c(vec1, vec2)
cnt <- 0

for(i in 1:1000000){
  x <- split(sample(vec3), rep(1:2, c(8,10)))
  res <- mean(x[[1]]) - mean(x[[2]]) 
  if(res > estimator){
    cnt <- cnt + 1
  }
}

p_value <- cnt / 1000000
p_value
#Zhetessov Nur