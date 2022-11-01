n1 <-15
xbar1 <-310 
s1 <-18.5
n2 <-15
xbar2 <-300
s2 <-16.4

sp = ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2)
margin <-qt(0.975,df=n1+n2-1)*sqrt(sp/n1 + sp/n2)

low <-(xbar1-xbar2) -marginhigh <-(xbar1-xbar2) + margin