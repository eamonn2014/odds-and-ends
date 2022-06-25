
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
##work question re combinations

x <- c(5,5,3,2,5,5)
res <- c()

# example we are selecting one group so there are 5,5,3,2,5,5 ways
combn(x,1) # we add these up to give 25, so there are 25 extra groups created
combn(x,2) # WHEN i IS 2 WE SAY WE ARE UsING 2 OPTIONAL GROUPS, THERE ARE 15 WAYS OF CHOOSING 2 GROUPS FROM 6

# now go through all, so there could be 1,2,3,4,5 or 6 groups, collect the information

for (i in 1:6) {
   z <- combn(x,i)  # WHEN i IS 1 THE NO OF WAYS OF SELECTING 1 FROM EACH GROUP = N
                    # WHEN i IS 2 WE SAY WE ARE UsING 2 OPTIONAL GROUPS, THERE ARE 15 WAYS OF CHOOSING 2 GROUPS FROM 6
   # WHEN i IS 2 WE SAY WE ARE UsING 2 OPTIONAL GROUPS, THERE ARE 15 WAYS OF CHOOSING 2 GROUPS FROM 6
   res[i] <- sum(apply(z,2,prod)) # prod of cols gives combinations, then sum them
}

res
sum(res)

# considering the core group there are 384 combinations
x <- c(4,4,4,2,3)
z <- combn(x,5)
zz <- sum(apply(z,2,prod))


sum(res+zz)  # answer


##########simulation to check exact answer


Y1 <- paste0("Y1",1:5)
Y2 <- paste0("Y2",1:5)
X1 <- paste0("X1",1:3)#

X2 <- paste0("X2",1:4)#
X3 <- paste0("X3",1:4)#
X4 <- paste0("X4",1:4)#
X5 <- paste0("X5",1:2)#
X6 <- paste0("X6",1:3)#

X7 <- paste0("X7",1:2)#
Y3 <- paste0("Y3",1:5)#
Y4 <- paste0("Y4",1:5)#


x <- c(5,5,3,2,5,5,4,4,4,2,3)
 
require(tidyverse)

# # sample 1 from each group
# xx <- function(){
#   
#   ss <- df %>%
#     group_by(g) %>%
#         sample_n(size=1)
#   return(ss$grade)
#   
# }


df <- data.frame(grade =c( Y1, Y2, X1,X2,X3,X4, X5,X6, X7,Y3,Y4))

# make the df


# z <- replicate(1000, xx())
# zz <- t(apply(z,1,sort))
# zzz<- (unique(data.frame(t(zz))))
# 
# 
# dim(zzz)
# 

#___________________________________________________________________________________________

# boot.cluster <- function(x, id){
#   boot.id <- sample(unique(id), replace=F)
#   out <-  lapply(1:length(boot.id),
#                  function(newid){cbind(x[id%in%boot.id[newid],],newid)})
#   return(do.call("rbind", out))
# }
# 
#  
#   w2 <- boot.cluster(x=df, id=df$grade)
  
  
  # plyr::arrange(df, grade)
  # plyr::arrange(w2, grade)

  #----------------------------------------------------
  # # select 1 or more groups randomly from df
  # x <- sample(1:length(unique(df$g)),1)
  # grp <- sample(unique(df$g), x,  replace=F) 
  # # select the grps in the data frame
  # df1 <- df[df$g %in% grp,]
  #----------------------------------------------------
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
  
  # do the calculations based on the optional groups
  df <- data.frame(grade =c( Y1, Y2, X1, X7, Y3, Y4)) # we ignore the core x2:x6 which is always in the molecule
  df$g <- substr(df$grade,1,2)
  df$y <- substr(df$grade,3,3)
  df
  
  #----------------------------------------------------
  # too slow
  # xx1 <- function(){
  #   
  #   x <- sample(1:length(unique(df$g)),1)       # select from 1:6, to choose groups or more groups randomly from df
  #   # so x is one of 1:6
  #   
  #   grp <- sample(unique(df$g), x,  replace=F)  # now select randomly x grps
  #   
  #   df1 <- df[df$g %in% grp,]  # subset the df based on the selected groups
  #   
  #   # now sample an 'amino acid' randomly from each group  
  #   ss <- df1 %>%
  #     group_by(g) %>%
  #   sample_n(size=1)
  #   
  #   return(ss$grade) #return the molecule
  #   
  # }
  # 
  # #----------------------------------------------------
  # 
  # # aim is to over sample so I have encoutered all the unique molecules
  # set.seed(2343)
  # z <- replicate(10000, xx1())   #create many molecules 
  # # sort row independently, so we can run the unique function
  # z <-  z[order(sapply(z,'[[',1))]  # https://stackoverflow.com/questions/28100593/how-to-sort-a-list-of-lists-in-r
  # # how many unique molecules are there?
  # z <- unique(z)
  # length(z)
  # 

  #----------------------------------------------------
  # DT package is much faster
  require(data.table)

  xx <- function(){
    
    # select from 1:6, then choose this no. of groups randomly from available groups
    x <- sample(unique(df$g), sample(1:6, 1),  replace=F)
    
    df1 <- df[df$g %in% x,]  # subset the d.f. based on the selected groups above
    
    setDT(df1)  # make a data table
    
    # now select randomly 1 amino-acid from each group, this is the molecule
    #https://stackoverflow.com/questions/16289182/sample-random-rows-within-each-group-in-a-data-table
    
    d <- df1[,.SD[sample(.N, min(1,.N))],by = g]
    
    return(d$grade) #return the molecule
    
  }


  set.seed(23413)
  sims <- 1e6                       # number of simulations
  system.time(z <- replicate(sims, xx()))        # create many molecules using function xx
  
                                    # sort rows independently of each other, so we can run the unique function
  z <-  z[order(sapply(z,'[[',1))]  # https://stackoverflow.com/questions/28100593/how-to-sort-a-list-of-lists-in-r
                                   
  z <- unique(z)                    # how many unique molecules are there?
  length(z)                         # this should be close to the empirical result, yes exactly 15551 combinations


























