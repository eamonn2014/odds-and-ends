

# Poisson calculations

# It is known that a certain website makes mu
# sales per hour. In a given hour, what is the probability that the site makes exactly x sales?
  
  
  mu =10
  x=8 

  dpois(x=x, lambda=mu)

  exp(-mu) * mu^x  / factorial (x)
  
  # poisson paper 
  ppois(q=x, lambda=mu) - ppois(q=x-1, lambda=mu)
  
 # at least x
  res <- c(rep(NA,x))
  for (i in 1:x ) res[i] <- mu^i/factorial(i)
  p1 <- ( 1 + sum(res ) ) * exp(-mu)

  # at least x-1  
  x=x-1
  res <- c(rep(NA,x))
  for (i in 1:x ) res[i] <- mu^i/factorial(i)
  p2 <- ( 1 + sum(res ) ) * exp(-mu)
  
  
  # probability of exactly x
  p1-p