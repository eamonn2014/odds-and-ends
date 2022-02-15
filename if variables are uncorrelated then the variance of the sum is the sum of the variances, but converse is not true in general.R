


n     <- 100
r     <- 0
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
n <- length(before)
d <- data.frame(y = c(before, after), 
                x = rep(c(1,2), each=n),
                id = factor(rep(1:n,2)))

d$xj <- jitter(d$x, amount=.03)
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

var(A) + var(B) - 2 * cor. * sd(A) * sd(B)  # CALCULATION 2

sd.^2 +sd.^2 - 2 * r * sd.*sd.   # TRUTH


