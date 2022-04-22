
#'trying neg binomial simulation'

# failure <- success <- 0
# 
# nd <- function(p=.5) {
# 
#   x <- rbinom(1, size=1, prob=p)
#   
#   if (x==0) {failure =failure+1 } else {success=success+1 }
#  
#   return(list(s=success, f=failure))
# }
# 
# res <- replicate(10, nd())




foo <- function(p = 0.1) {
  i <- 0
  failure <- TRUE
  while ( sum(failure )==1 ){
    i <- i + 1
    if ( sample(x = c(TRUE, FALSE), size = 1, prob = c(p, 1-p)) ) { # equiv to if coin flip is heads
      failure <- FALSE
    }
  }
  return(i) 
}

set.seed(42)
number_of_attempts <- replicate(1000, foo())
hist(number_of_attempts, xlab = "Number of Attempts Until First Success")



# same as 
# https://stackoverflow.com/questions/65462039/r-how-would-i-repeatedly-simulate-how-many-attempts-before-a-success-on-a-1-10

foo <- function(p = 0.1) {
  i <- 0
  failure <- 1
  
  while ( failure  ){
    i <- i + 1
    if ( rbinom(1, size=1, prob=p))  { # equiv to if coin flip is heads
      failure <- 0 # if not heads 0
    }
  }
  return(i) 
}

set.seed(42)
number_of_attempts <- replicate(1000, foo())
hist(number_of_attempts, xlab = "Number of Attempts Until First Success")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# my neg binomial simulation
# for example if s = 4, we count how long it takes to get 4 heads given prob of p 

formatz2 <- function(x){
  sprintf(x, fmt = '%#.2f')  
}

foo <- function(p = 0.5, s = 4) { #p prob of success; s 
  
  i <- 0
  x <- 0 
  
  while ( x < s) {  # stop when x equals s 
    
    i <- i + 1  # count each trial
    
    res <-  rbinom(1, size=1, prob=p)  # ala coin flip, heads is success
    
    if (res ==1) {x=x+1}   # if heads count 
   
  }
  return(i) 
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ plot
# ref
# https://bookdown.org/mpfoley1973/data-sci/negative-binomial.html see for explanation
# Function dnbinom() calculates the negative-binomial probability. Parameter x equals the number of failures, x−r.

  sim = 1000
  P <- runif(1,0,1)  # pick a probability
  s <- sample(2:10,1) # pick nth success
  
  # simulation bar plot
  number_of_attempts <- replicate(sim, foo(p=P,s=s))
  x <- table(factor(number_of_attempts, levels = s: max(number_of_attempts))) # pad out so levels no counts included
  b <- barplot(x/sim, main = paste0("Number of attempts until we reach ",s," successes, p=",formatz2(P),""), 
               col="palegreen", ylab="Probability", xlab="Attempt (true negative binomial dist. blue line)")
  
  # overlay true neg binomial dist
  m <- length(b)-1
  d <- dnbinom(0:m, prob=P, size=s)# *sim
  lines(x = b, y = d , col='blue')
  points(x = b, y = d)
  # bar plot(d, names.arg= as.character(names(x)))
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 