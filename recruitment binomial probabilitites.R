
n<-7#29
p<-.5#0.45
k<-3#8

###after n what can we expect with success prob of p

x<-rbinom(n, n=10000, prob=p)
barplot(table(x), col='blue')
quantile(x, c(0.025,0.5, 0.975))

###another way

qbinom(c(0.025,0.5,0.975), n, p)

##############

library(binom)
binom.confint(k, n, conf.level = 0.95, methods = "wilson")
 

# equivalences
factorial(n)
exp(lfactorial(n))
gamma(n+1)
exp(lgamma(n+1))

# relationships
log(choose(n,k))
lgamma(n+1)-lgamma(n-k+1)-lgamma(k+1)
lgamma(n+1)-(lgamma(n-k+1)+lgamma(k+1))


 
choose(n,k)*(p^k)*(1-p)^(n-k)

f<-factorial(n)/((factorial(n-k)*factorial(k)))
f*(p^k)*(1-p)^(n-k)

f<-lfactorial(n)-((lfactorial(n-k)+lfactorial(k)))
exp(f)*(p^k)*(1-p)^(n-k)

f<-lgamma(n+1)-((lgamma(n-k+1)+lgamma(k+1)))
exp(f)*(p^k)*(1-p)^(n-k)




# ##make a factorial function.
# ##probability of getting exactly k in n 
# factorialz <- function (x) gamma(1 + x) 
# 
# n<-n
# k<-9
# f<-factorialz(n)/((factorialz(n-k)*factorialz(k)))
# pi<-p
# p<-(pi^k)*(1-pi)^(n-k)
# f*p

 
# 
# 
# 
# 
# 
# f<- lgamma(n+1) - lgamma(n-k+1) + lgamma(k+1)
# f <- lfactorial(n) - lfactorial(n-k) + lfactorial(k)
# 
# pi<-0.45
# p<-(pi^k)*(1-pi)^(n-k)
# exp(f)*log(p)
#  
# 
# ##another way
# choose(n,k)*(pi^k)*(1-pi)^(n-k)
# 
# 
# 
# 
# choose(n,k)
# 






###on average(median) in 20 trials what success prob will deliver 20*.3=6 eligibles
median<-0.5
trials<-20
qbinom(median, trials, c(0.3,0.4,0.45,0.55))


##if I want to recruit N how many tests do I need to run.
N<-3  
f<-0.3 
dist<-(rnbinom(1e6 , N, f) + N )  
hist(dist , xlab='Total Number of Tests')
mean(dist)
rang<-1:100/100
quantile(dist, rang)
quantile(dist, c(0.025, 0.25, 0.5, 0.75, 0.975))

library(binom)
binom.confint(4, 19, conf.level = 0.95, methods = "wilson")
qbinom(c(0.025,0.5,0.975), 19, 0.3)