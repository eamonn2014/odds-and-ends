
#https://stats.stackexchange.com/questions/136806/expected-number-of-tosses-till-first-head-comes-up
p <- 1/3                                           # Set the chance of heads
tosses <- runif(1e3) < p                           # Make a million tosses
sim <- diff(c(TRUE, which(tosses)))                # Compute game lengths
hist(sim, xlab="Game length", main="Distribution") # Graph their distribution
mean(sim)                                          # Report the average len


#https://stats.stackexchange.com/questions/276454/how-to-simulate-poisson-arrival-times-if-the-rate-varies-with-time
rates <- c(1.5, 2.1, 3.4)
rates <- c(1.5, 1.5, 1.5)
arrival_times <- c()
for (hour in 1:3000) {
  n_arrivals <- rpois(1,rates[1 + hour%%3])
  arrival_times <- c(arrival_times, runif(n_arrivals) + hour - 1)
}
arrival_times <- sort(arrival_times)


#file:///C:/Users/Lenovo/OneDrive/Documents/BOOKS/STATISTICS/COUNTS/useful%20for%20neg%20binomial/Maria%20L.%20Rizzo%20-%20Statistical%20computing%20with%20R.%201-Chapman%20&%20Hall%20_%20CRC%20(2007)%20page%2044.pdf
# check example 3.25

#--------------------------------------------------------------------------------------------------------
# work question re combinations

x <- c(5,5,3,2,5,5) # 6 groups and their sizes
res <- c()

# example we are selecting one group so there are 5,5,3,2,5,5 ways
combn(x,1) # we add these up to give 25, so there are 25 extra groups created
combn(x,2) # WHEN i IS 2 WE SAY WE ARE UsING 2 OPTIONAL GROUPS, THERE ARE 15 WAYS OF CHOOSING 2 FROM 6

# now go through all, so there could be 1,2,3,4,5 or 6 groups, collect the information

for (i in 1:6) {
   z <- combn(x,i)  # WHEN i IS 1 THE NO OF WAYS OF SELECTING 1 FROM EACH GROUP = N
                    # WHEN i IS 2 WE SAY WE ARE USING 2 OPTIONAL GROUPS, THERE ARE 15 WAYS OF CHOOSING 2 GROUPS FROM 6
   # WHEN i IS 3 WE SAY WE ARE UsING 3 OPTIONAL GROUPS, THERE ARE 20 WAYS OF CHOOSING 3 GROUPS FROM 6
   res[i] <- sum(apply(z,2,prod)) # prod of cols gives combinations, then sum them all
}
res
sum(res)


# considering the core group there are 384 combinations
x <- c(4,4,4,2,3)
z <- combn(x,5)
zz <- sum(apply(z,2,prod))

sum(res+zz)  # answer


#------------------------------------------------------------------------------------------------------------
# simulation to check exact answer
#------------------------------------------------------------------------------------------------------------

  require(tidyverse)

  # make df based on instructions
  Y1 <- paste0("Y1",1:5) # optional
  Y2 <- paste0("Y2",1:5) # optional
  X1 <- paste0("X1",1:3) # optional
  
  X2 <- paste0("X2",1:4)# core
  X3 <- paste0("X3",1:4)# core
  X4 <- paste0("X4",1:4)# core
  X5 <- paste0("X5",1:2)# core
  X6 <- paste0("X6",1:3)# core
  
  X7 <- paste0("X7",1:2)# optional
  Y3 <- paste0("Y3",1:5)# optional
  Y4 <- paste0("Y4",1:5)# optional
  
  # do the simulation based on the optional groups only
  df   <- data.frame(grade =c( Y1, Y2, X1, X7, Y3, Y4)) # we ignore the core x2:x6 which is always in the molecule
  df$g <- substr(df$grade,1,2)
  df$y <- substr(df$grade,3,3)
  df
  
  #-------------------------------------------------------------------------------------------------------------
  # DT package is fast for stratified sampling
  require(data.table)

  xx <- function(){
    
    # select from 1:6, then choose this no. of groups randomly from available groups
    x <- sample(unique(df$g), sample(1:6, 1),  replace=F)
    
    df1 <- df[df$g %in% x,]  # subset the d.f. based on the selected groups above
    
    setDT(df1)  # make a data table
    
    # now select randomly 1 amino-acid from each group, this is the molecule
    # https://stackoverflow.com/questions/16289182/sample-random-rows-within-each-group-in-a-data-table
       
    d <- df1[,.SD[sample(.N, min(1,.N))],by = g]   # DT stratified sampling
    
    return(d$grade) #return the molecule
    
  }

  #----------------execute function and manage-------------------------------------------------------------------
  
  set.seed(23413)
  sims <- 1e6                                    # number of simulations
  system.time(z <- replicate(sims, xx()))        # create many molecules using function xx()
                                    # sort rows independently of each other, so we can run the unique function
  z <-  z[order(sapply(z,'[[',1))]  # https://stackoverflow.com/questions/28100593/how-to-sort-a-list-of-lists-in-r
                                   
  z <- unique(z)                    # how many unique molecules are there?
  length(z)                         # this should be close to the empirical result, yes exactly 15551 combinations

  #----------------------------------------------------------------------------------------------------------------
























