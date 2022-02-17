
# correlated normals!!
rm(list=ls())
#set.seed(7789)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# another function to generate cor data , see count app
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mvrnorm <- function(n = 1, mu = 0, Sigma) {
  nvars <- nrow(Sigma)
  # nvars x n matrix of Normal(0, 1)
  nmls <- matrix(rnorm(n * nvars), nrow = nvars)
  # scale and correlate Normal(0, 1), "nmls", to Normal(0, Sigma) by matrix mult
  # with lower triangular of cholesky decomp of covariance matrix
  scaled_correlated_nmls <- t(chol(Sigma)) %*% nmls
  # shift to center around mus to get goal: Normal(mu, Sigma)
  samples <- mu + scaled_correlated_nmls
  # transpose so each variable is a column, not
  # a row, to match what MASS::mvrnorm() returns
  t(samples)
}


n     <- 100
r     <- -.4
mu0   <- 0
eff.p <- 10
sd.   <- 10

L1 <- mu0
L2 <- mu0+eff.p

# generate correlated poisson ref: https://thomasward.com/simulating-correlated-data/
# Sample correlated N(0, 1) distributions from a multivariate normal distribution.
# Transform them to correlated Uniform(0, 1) distributions with the normal CDF.
# Transform them to any correlated probability distribution you desire with that probability distribution’s inverse CDF.

Sigma <- matrix(c(1, r, r, 1), 2, 2)

p2 <- mvrnorm(n, Sigma = Sigma)   # correlated continuous, see function in global area!

U <- pnorm(p2, mean = 0, sd = 1)  # correlated uniform
pp1 <- qnorm(U[, 1], L1, sd.)      # correlated normal
pp2 <- qnorm(U[, 2], L2, sd.)      # correlated normal

cor. <- cor(pp1,pp2)              # capture correlation
cor.

# create a data frame
my_data <- data.frame( 
  group = rep(c("A.before", "B.after"), each = n),
  counts = c(pp1,  pp2),
  ID=rep(1:n,2)
)

## variance of a difference

var(pp1) + var(pp2) - 2 *cor. * sd(pp1) * sd(pp2)

sd.^2 +sd.^2 - 2 * r * sd.*sd.

## variance of a sum
var(pp1) + var(pp2) + 2 *cor. * sd(pp1) * sd(pp2)

sd.^2 +sd.^2 + 2 * r * sd.*sd.

r


###plot

library(ggplot2)

A <- pp1
B <- pp2
m <- ceiling(max(A,B))
mL <- floor(min(A,B))
before <- A + runif(length(pp1),-.2,.2)
after <-  B + runif(length(pp2),-.2,.2)

before <- A #+ 0
after <-  B #+ runif(length(pp2),-.2,.2)

n <- length(before)
d <- data.frame(y = c(before, after), 
                x = rep(c(1,2), each=n),
                id = factor(rep(1:n,2)))

d$xj <- jitter(d$x, amount=.13)
# code from count app
AA <- ggplot(data=d, aes(y=y) ) +
  geom_boxplot(aes(x=x, group=x), width=0.2, outlier.shape = NA, col='blue', fill='lightblue') +
  geom_line(aes(x=xj, group=id),  colour='pink', alpha=.5) +
  geom_point(aes(x=xj), size=2) +
  xlab("Phase") + ylab("Count") +  
  scale_x_continuous(breaks=c(1,2), labels=c("Before", "After"), limits=c(0.5, 2.5)) +
 # scale_y_continuous(breaks=c(mL:m), limits=c(mL,m)) +
  theme_bw() + theme(legend.position = "none") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ scatter plot

library(gridExtra)

d <- data.frame(y = B , x = A)

BB <- ggplot(d, aes(x, y)) +
  geom_jitter(width = 0.1, height = 0.1, size=2) +
 #  scale_x_continuous(breaks=c(0:m), limits=c(mL,m)) +
 # scale_y_continuous(breaks=c(0:m), limits=c(mL,m)) +
  scale_x_continuous( limits=c(mL,m)) +
  scale_y_continuous(  limits=c(mL,m)) +
  xlab("Before count") +ylab("After count") +
  theme_bw() + theme(legend.position = "none") +
  geom_abline(intercept=0, slope=1)

plot1 <- AA
plot2 <- BB
grid.arrange(plot1, plot2, ncol=2)

#### variance of diff  = sum of variances (also gotta consider differences)

var(A-B) #CALCULATION 1, think of the variation in the lines on the boxplot

#hist(A-B, breaks=30)

var(A) + var(B) - 2 * cor. * sd(A) * sd(B)  # CALCULATION 2

sd.^2 +sd.^2 - 2 * r * sd.*sd.   # TRUTH

t.test(A,B, paired=TRUE)


library(MethComp)

my_data <- data.frame( 
  group = rep(c("before", "after"), each =n),
  counts = c(d[,1],  d[,2]),
  ID=rep(1:n,2)
)

m <- Meth(my_data,meth="group",item="ID",repl=NULL,y="counts",print=TRUE)

# get reasonable limits for y axis
# https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x/6468532#6468532

L <- max(abs(floor(min(A-B))),abs(ceiling(max(A-B))))

# nice function from link

  roundUp <- function(x,to=10)
  {
    to*(x%/%to + as.logical(x%%to))
  }

  L <- roundUp(L)
  
  # BA.plot( m, model=NULL, repl.conn=TRUE, col.lines="blue",
  #         # axlim=c(0,20), 
  #          diflim=c(-L,L), 
  #          xaxs="i", yaxs="i",
  #          las=1, eqn=TRUE, dif.type="lin", pl.type="BA", sd.type="lin", #cons
  #          grid=1:9*10, digits=3,font.eqn=1  , eqax=F)
  
  BA.plot( m, model=NULL, repl.conn=TRUE, col.lines="blue",
           # axlim=c(0,20), 
           diflim=c(-L,L), 
           xaxs="i", yaxs="i",
           las=1, eqn=TRUE, dif.type="lin", pl.type="BA", sd.type="lin", #cons
           grid=1:9*10, digits=3,font.eqn=1  , eqax=F)
  
  # we st up const dif and const sd so this matches simulation
  BA.plot( m, model=NULL, repl.conn=TRUE, col.lines="blue",
           # axlim=c(0,20), 
           diflim=c(-L,L), 
           xaxs="i", yaxs="i",
           las=1, eqn=TRUE,  pl.type="BA", sd.type="coms", dif.type = "const", # cons is simulatied
           grid=1:9*10, digits=3,font.eqn=1  , eqax=F)
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # code from my useful code, adjusted slightly

  BA <- DA.reg(m, DA.slope = TRUE)
  BA

  a1<-as.data.frame(unlist(BA))
  #DA.slope.p<-a1[14,]
  #cons.sd.p.value<-a1[38,]
  
  res<-rbind(a1[14,], a1[38,])
  res<-as.matrix(res)
  
  rownames(res)<-c( 
                   "DA Slope P-value",
                   "P-value for Changing SD")
  
  colnames(res) <- c("Bland Altman Analysis")
  print(res)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## **Assess Bland Altman assumptions P-values for two hypotheses:**
  w <- to.wide(m)
  
  
  ## assumptions do it yourself

  w$y <- w$before - w$after
  w$x <-  (w$before + w$after) /2
  
  # Constant difference - this is the test of 0 slope in the regression of differences on averages.
  f <- summary(lm(y~x, w))
  f$coefficients["x","Pr(>|t|)"]
  
  
  # Constant variance - this is the test of 0 slope in the regression of absolute residuals on averages.
  w$r <- abs(residuals(f) ) # if you dont use abs the regression line slope will be 0!
 # w$r <- (residuals(f) )
  plot(r~x,w)
  f <- summary(lm(r~x, w))
  assump2 <- f$coefficients["x","Pr(>|t|)"]
  
  ##
  # . A formal test of increasing or
  # decreasing variance by the level of measurement can be obtained by
  # first regressing the differences on the averages and then regressing the
  # absolute residuals from this on the averages 
  # 
  library(ggplot2)
  library(ggpubr)
  ggplot(w, aes(x=x, y=r))+
    geom_point() +  
    geom_smooth(method="lm", col="black")  +
    stat_regline_equation(label.x = min(w$x), label.y = max(w$r))+
    theme_bw() +
    labs(title = paste0("Regression of absolute residuals on averages, p=", assump2),
         x = "Averages",
         y = "Absolute residuals")
  
  
  
  
  
  
  f <- with( w, lm( before- after ~ I((before + after)/2) ) )
  f1 <- with( w, lm( abs(residuals(f)) ~ I((before + after)/2) ) )
  summary( f1 )$coef
  
  
  # see altman paper
  # DG Altman. Calculating age-related reference centiles using absolute residuals. Statistics in Medicine, 12: 917–924, 1993.
  # The mean of a half standard normal distribution is sqrt(2/pi).Thus
  # the mean of the absolute residuals multiplied by sqrt(pi/2) is an estimate of the SD of the residuals. 
  mean(abs(residuals(f)))*sqrt(pi/2)
  sd(residuals(f))
  
  # why the above works
  # look at wikipedia mean of standard half normal =sigma X sqrt(2) / sqrt(pi)
  # if we multiply the above by sqrt(pi) / sqrt(2) we are left with sigma!
  # so knowing the mean we can find the SD!!!
  
  
  # an aside
  # checking out the mean sd relationship between stand normal and half standard normal
  mu=0      # 0 so half data is negative
  sigma=runif(1, 1,100)
  sigma

  x <- rnorm(10000, mean=mu, sd=sigma)  # note if this is all positive, abs value will make no difference, hence mu=0
  
  hn <- abs(x)
  # mean of half normal
  mean(hn)
  sigma*sqrt(2)/sqrt(pi)  # analytical expectation mean
  
  #sd of half normal
  sd(hn)
  sqrt(sigma^2*(1 - (2 / pi)))
  
  
  
  
  
  
    
  
  

