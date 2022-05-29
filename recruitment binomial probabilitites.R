
n<-29
p<-0.45
k<-8

###after n what can we expect with success prob of p

x<-rbinom(n, n=10000, prob=p)
barplot(table(x), col='blue')
quantile(x, c(0.025,0.5, 0.975))

###another way

qbinom(c(0.025,0.5,0.975), n, p)

##############

library(binom)
binom.confint(k, n, conf.level = 0.95, methods = "wilson")
 

##make a factorial function.
##probability of getting exactly k in n 
factorial <- function (x) gamma(1 + x) 

n<-n
k<-9
f<-factorial(n)/((factorial(n-k)*factorial(k)))
pi<-p
p<-(pi^k)*(1-pi)^(n-k)
f*p

##another way
choose(n,k)*(pi^k)*(1-pi)^(n-k)












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