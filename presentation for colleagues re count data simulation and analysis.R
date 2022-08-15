#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# simulate a single negative binomial RCT.
# accounting for trt discontinuation 
# enter one arm n, k, placebo rate, trt effect, drop off trt rates and follow up time
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rm(list=ls())
  set.seed(345)
  library(MASS)    
 
  n=220               # in each arm
  mu0=1; mu1=.65;     # placebo rate and rate in treated respectively
  drop1=.1; drop2=.1; # allow dropouts
  fup=1; k=.8         # follow up time and k
  
  # just so there is no error if drop out or k=0
  
  if (drop1==0) {drop1=0.0001}
  if (drop2==0) {drop2=0.0001}
  if (k==0) {k=0.0001} 
  
  dose <- c(rep("placebo",n),rep("trt",n)) # 50:50 split of patients
  
  mu   <- c(rep(mu0,n), rep(mu1,n))        # rates in two arms
  
  drop <- c(rep(drop1,n), rep(drop2,n))    # tr discontinuation rates
  
  f <- - rexp(2*n) / log(1-drop/fup)       # generate drop outs and scale time according to follow up!
  
  indiv.time <- ifelse(f > fup, fup, f)    # curtail at follow up time 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # this parameterization see MASS::rngebin code
  theta <- 1/k
  y <- rpois(2*n, (indiv.time* mu *        rgamma(2*n, theta))/theta) +
       rpois(2*n, ((fup-indiv.time)* mu0 * rgamma(2*n, theta))/theta)
  
  # assume no one is lost to the study but can discontinue treatment
  logtime  <- rep(log(fup), n*2)          # make sure follow up here !
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # analyse with neg. binomial model
  mod <- glm.nb(y~dose+offset((logtime)))
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # post modelling
  x <- summary(mod)
  x   # look at theta , this is 1/k!!
  exp(x$coefficients["dosetrt","Estimate"])   # trt effect
  
 
  t1 <- y[dose=="trt"]
  t0 <- y[!dose=="trt"]
  z <- max(max(table(t0), max(table(t1))))*1.1  # y axis max shared
  
   # plot counts
  par(mfrow=c(1,2))
  barplot(table(t1), main="treatment" ,  ylim=c(0, z), col="blue")
  barplot(table(t0), main="placebo",  ylim=c(0, z), col="green")
  par(mfrow=c(1,1))
  
  #summary(t1); var(t1) 
  #summary(t0); var(t0)
  
  mean(t1)/mean(t0)   # treatment effect
  
  #(var(y)-mean(y))/ mean(y)^2  # estimate of k see slide 22
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  