



rm(list=ls())
mu=10
xmax <-25
ymax <- 0.15
xx <- c(seq(0,xmax,1))

yb = dpois(xx, mu)

par(mar=c(2.1,4.1,4.1,4.1))

b <- barplot(yb, xlim=c(0,xmax+2), ylab = "Probability", xlab="x", col='black', ylim=c(0,ymax),
             main = paste0("Poisson \u03BC = ", mu, sep=""),
              names.arg= as.character(xx))

 x <- 0:length(b)    #  % on top of bars
 y1 <- yb + 0.01
 z <- round(yb,2) *100
 z <- paste0(z,"%")
 text(x=b[x+1],y=(y1),labels=z, cex = .75)
