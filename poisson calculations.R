
# he “Poisson” Distribution: History, Reenactments,Adaptations
# Poisson calculations

# It is known that a certain website makes mu
# sales per hour. In a given hour, what is the probability that the site makes exactly x sales?
  
  
  mu =10
  x=8 

  # pmf
  dpois(x=x, lambda=mu)

  exp(-mu) * mu^x  / factorial (x)
  
  # use cdf to answer the question
  ppois(q=x, lambda=mu) - ppois(q=x-1, lambda=mu)
  
  # code up cdf
  res <- c(rep(NA,x))
  for (i in 1:x ) res[i] <- mu^i/factorial(i)
  p1 <- ( 1 + sum(res ) ) * exp(-mu)

  # code up cdf, at least x-1  
  x = x-1
  res <- c(rep(NA,x))
  for (i in 1:x ) res[i] <- mu^i/factorial(i)
  p2 <- ( 1 + sum(res ) ) * exp(-mu)
  
  # probability of exactly x
  p1-p2
  
  mu =10
  x=8 
  dnorm(mean=mu, sd=x^.5, x=x)
  
  
  x = 200 ; mu = 1:2000
  p <-  dpois(x=x, lambda=mu)
  n <- dnorm(mean=mu, sd=x^.5, x=x)
  
  
  plot(p,n, main="sample size")
  abline(0,1)
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Newcombe 1860 prob that, if the heavens were divided at random in square degrees, 
  # 'some' one of those square degrees would contain six stars
  
  stars <- 1500
  squares <- 41253
  mu <- stars/squares            # 0.03636
  dpois(x=6, lambda=mu)*squares  # 0.000000128

  # used Poisson to determine "probability that, if the stars were scattered
  # at random over the heavens, any small space selected at random would contain s stars"
  # he also said "this, however, is evidently rather smaller than the probability that 6 starts
  # should be found so near together that a square degree could be fitted on as to include them"
  
  # lets simulate 1500 from 41253
  z <- sample(x=41253, size=1500 , replace=TRUE)
   
  addmargins(table((as.vector(table(z)))))
  
  print("Count of repeated values")
  g1a <- length(which(table(z)>1))
  
  print("Elements which are repeated")
  g1b <- which(table(z)>1)
  
  
  # z1 <- rle(z)  # 1500
  # addmargins(table((as.vector(table(z)))))
  # as.numeric(names(table(z)))  # cells with  or more stars
  
  
  stars <- function(x.=1500, size.=41253) {
    
    z <- sample(x=x., size=size. , replace=TRUE)
    res <- length(which(table(z)==6))
    
    return(res)
    
  }
  
  s <- replicate(1000, stars())
  mean(s)
  
 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # probability of observing no events! simplifies to e^-
  
  # simulation
  # ramdomly
  
  #  obtain 10 random observations from a Poisson distribution with mean 4 
  z <- rpois(1e8, lambda = stars/squares )
  table(z)
  

  
  
  
  
  
  
  