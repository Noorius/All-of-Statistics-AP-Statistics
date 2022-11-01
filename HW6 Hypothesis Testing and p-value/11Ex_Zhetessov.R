exp.data <- data.frame(
  name = c ("Placebo", "Chlorpromazine", "Dimenhydrinate", "Pentobarbital (100 mg)", "Pentobarbital (150 mg)"), 
  total = c(80, 75, 85, 67, 85),
  nausea = c(45, 26, 52, 35, 37)
)

p_hat <- exp.data$nausea/exp.data$total #Вычисляю пропорции
p_hat

delta_hat <- p_hat[2:5] - p_hat[1] #Вычисляю estimator дельта_hat (Лекарства - Плацебо)
delta_hat

SE_hat <- sqrt( (p_hat[1]*(1-p_hat[1])/exp.data[1,]$total) + (p_hat[2:5]*(1-p_hat[2:5])/exp.data[c(2:5),]$total) ) 
#Вычисляю стандартное отклонение для всех лекарств
SE_hat

W <- delta_hat / SE_hat
W

p_values <- 2 * pnorm(-abs(W))
p_values

reject_H0 <- c()
for(i in W){
  if(abs(i)>1.96){
    reject_H0 <- append(reject_H0, i)
  }
}
reject_H0

#Zhetessov Nur