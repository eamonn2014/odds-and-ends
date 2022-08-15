
# negative binomial simulation for nth success.

# chunk 1 from
# https://stackoverflow.com/questions/65462039/r-how-would-i-repeatedly-simulate-how-many-attempts-before-a-success-on-a-1-10

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



# chunk 2
# I revamped the above to reproduce it

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
# chunk 3
# my neg binomial simulation
# for example if s = 4, we count how long it takes to get 4 heads given prob of p 

formatz2 <- function(x){
  sprintf(x, fmt = '%#.3f')  
}

foo <- function(p = 0.5, s = 4) { # p prob of success; s 
  
  i <- 0 # count of how many attempt we need to get s successes
  x <- 0 # use this to know when to stop function
  
  while ( x < s) {  # stop when x equals s 
    
    i <- i + 1  # count every attempt
    
    res <-  rbinom(1, size=1, prob=p)  # ala coin flip, heads is success
    
    if (res ==1) {x=x+1}   # if heads count , if not we fail
   
  }
  return(i) # result captured here
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ plot
# ref
# https://bookdown.org/mpfoley1973/data-sci/negative-binomial.html see for explanation
# Function dnbinom() calculates the negative-binomial probability. Parameter x equals the number of failures, x−r.

  sim <- 10000
  P <- runif(1,0.05,1)   # randomly pick a probability
  s <- sample(2:10,1)    # randomly pick nth success
 
  #s<-100
  # simulation bar plot
  number_of_attempts <- replicate(sim, foo(p=P,s=s))  # run function many times
  
  Tm <- s*(1-P)/P + s
  Tm <-  1/P*s 
  Sm <- mean(number_of_attempts) 
  
 
  x <- table(factor(number_of_attempts, levels = s: max(number_of_attempts))) # pad out so levels so no counts included
  b <- barplot(x/sim, main = paste0("Number of attempts until we reach ",s," successes when p=",formatz2(P),
                                    "\n True mean ",formatz2(Tm) ," ; Simulated mean ",formatz2(Sm) ,""
                                    
                                    ),
                                    
               col="palegreen", ylab="Probability of success", xlab="Attempt (true negative binomial dist. blue line)")
  
  # overlay true neg binomial dist.
  m <- length(b)-1
  d <- dnbinom(0:m, prob=P, size=s)# *sim
  lines(x = b, y = d , col='red')
  points(x = b, y = d, col='red')
  #abline(v=Tm, col='blue', lty=3)
  #abline(v = 0.7 +Tm*1.2, col = "red")
  # bar plot(d, names.arg= as.character(names(x)))
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
   s*(1-P)/P + s
   1/P*s #!
   
   mean(number_of_attempts)
   my_dat <- rnbinom(n = 10000, size = s, prob = P)
   mean(my_dat) +s
  
  
  
  
  
  
 