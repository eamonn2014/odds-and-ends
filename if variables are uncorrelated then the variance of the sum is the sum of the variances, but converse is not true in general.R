
n <- 10000
r <- 0
mu0   <- 0
eff.p <- 10
sd. <-10

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


var(pp1) + var(pp2) - 2 *cor. * sd(pp1) * sd(pp2)

sd.^2 +sd.^2 - 2 * r * sd.*sd.


var(pp1) + var(pp2) + 2 *cor. * sd(pp1) * sd(pp2)

sd.^2 +sd.^2 + 2 * r * sd.*sd.







