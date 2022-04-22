
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




# my neg binomial simulation
# for example if s = 4, we count how long it takes to get 4 heads given prob of p 

foo <- function(p = 0.5, s = 4) { #p prob of success; s 
  
  i <- 0
  x <- 0 
  
  while ( x < s) {  # stop when x equals s 
    
    i <- i + 1  # count each toss
    
    res <-  rbinom(1, size=1, prob=p)  # coin flip, heads is success
    
    if (res ==1) {x=x+1}   # if heads count 
   
  }
  return(i) 
}


as.data.frame(xtabs(length~timeSlt, dt))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#par(mfrow = c(2, 1))    
  #set.seed(42)
  sim = 1000
  P= runif(1,0,1) # pick a probability
  s=4
  number_of_attempts <- replicate(sim, foo(p=P,s=s))
  x <- table(factor(number_of_attempts, levels = s: max(sort(unique(number_of_attempts)))))
  #hist(number_of_attempts, xlab = "Number of Attempts Until x Success", breaks=100)
  b <- barplot(x, xlab = paste0("Number of Attempts Until ",s," Success, p=",P,""))
  

  
  
  
  m <- length(b)-1
  # https://bookdown.org/mpfoley1973/data-sci/negative-binomial.html see for explanation
  #Function dnbinom() calculates the negative-binomial probability. Parameter x equals the number of failures, x−r.
  d <- dnbinom(0:m, prob=P, size=4)*sim
  
  lines(x = b, y = d)
  points(x = b, y = d)
  
  # barplot(d, names.arg= as.character(names(x)))
 
#par(mfrow = c(1, 1))    

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


plot(b+0.5, d, "l", lwd = 2,col="red",lty=1, 
     axes=F, #ylim=c(0, 14000),
     #xlim=c(min(b), max(b)+1))
)
points(b+0.5, d, pch = 18, cex = 1,col="red")


# success <- 21
# attempt <- 30
# fail <- attempt - success
# streak <- function()
# {
#   success.pos <- sort(sample(1:attempt, success))
#   fail.pos <- sort((1:attempt)[-success.pos])
#   streak <- 0
#   prev <- success.pos[1]
#   for(i in success.pos[-1])
#   {
#     current <- i
#     if(prev == current-1)
#     {
#       streak <- streak + 1
#     }
#     prev <- current
#   }
#   
#   prev <- fail.pos[1]
#   for(i in fail.pos[-1])
#   {
#     current <- i
#     if(prev == current-1)
#     {
#       streak <- streak + 1
#     }
#     prev <- current
#   }
#   return(streak/attempt)
# }