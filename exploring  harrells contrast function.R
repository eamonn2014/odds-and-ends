

rm(list=ls())
library(rms)

N =4000
x1 <- N  # size
x2 <- 5000
x3 <- 100     # mean
x5 <- 10  # sd
x6 <- 10

zx1 <- 2
zx2 <- 4
zx3 <- 6

zx1 <- exp(0.693); zx2 <- exp(.693*2); zx3 <- exp(.693*3)

delta1 <- log(zx1)
delta2 <- log(zx2)
delta3 <- log(zx3)
 

top <-  4  # number of groups

#----------------------------------------------------
# seems that I need to use both c(x1,x2) c(x1:x2) so sample function works correctly

if (x1==x2) {
  
  middle <-  sample(c(x1,x2),   top, replace=TRUE)    # choose top count between sliders 
  
} else {
  
  middle <-  sample(c(x1:x2),   top, replace=TRUE)    #  
}

#----------------------------------------------------
lower <-   x3# groups mean
#----------------------------------------------------

if (x5==x6) {
  
  replicates <-  sample(c(x5,x6),  top, replace=TRUE )   #group sds
  
} else {
  
  replicates <-  sample(c(x5:x6),   top, replace=TRUE )   #grp sds
  
}
#----------------------------------------------------

# i dont like the naming of these functions!!
# try these functions https://github.com/davidski/collector/blob/main/R/fit_distributions.R

#' Given a set of parameters describing a lognormal distribution, return
#'   the parameters of the underlying normal distribution.

lognormal_to_normal <- function(meanlog, sdlog) {
  norm_mean <- exp(meanlog + sdlog^2 / 2)
  norm_sd <- sqrt( (exp(sdlog^2) - 1) * exp(2*meanlog + sdlog^2))
  list(mean = norm_mean, sd = norm_sd)
}

#' normal_to_lognormal(normmean = 20, normsd = 3)
normal_to_lognormal <- function(normmean=10, normsd=2) {
  phi <- sqrt(normsd ^ 2 + normmean ^ 2)
  lognorm_meanlog <- log(normmean ^ 2 / phi)
  lognorm_sdlog <- sqrt(log(phi ^ 2 / normmean ^ 2))
  list(meanlog = lognorm_meanlog, sdlog = lognorm_sdlog)
}


temp <- rep(lower, top)
#-----------------------------------------------------------------------

x <- matrix(data = NA, nrow = 4, ncol = 2, byrow = FALSE,
            dimnames = NULL)

#transform to normal distributions
for( i in 1:4) {
  x[i,] <- unlist(normal_to_lognormal(normmean=temp[i], normsd=replicates[i]))
}

# get the sds
replicates <- x[,2]  

x[1,1] -> mu  # intercept
lower <- c(mu, mu+delta1, mu+delta2, mu+delta3)  # true means

#-----------------------

Nj    <- sum(middle)                        # sum each group size 

muJ   <- rep(lower, times=rep(middle))      # expand means by group sizes

sds   <- rep(replicates, times=rep(middle)) # expand sd by group sizes

grpnames <- LETTERS[1:4]

IV <- factor( rep( grpnames, times=rep(middle) ) )

d <- data.frame(IV=IV,
                mu= muJ, 
                sd= sds,
                x=1
)


d$DV = rlnorm(d$x, meanlog=d$mu, sdlog = d$sd)  # create the response

df <- as.data.frame(d)

dd <- plyr::arrange(df, IV)    # sort and create for better order

dd$x <- NULL



ddz <<- datadist(dd) ;    options(datadist='ddz')
f <- ols(log(DV) ~ IV, df)
f
x0 <- summary(f)
x <-  exp(x0[,4])
x
# cc <- contrast(f, 

#getAnywhere(contrast.rms)
 
  fit <- f
  
  
  
  qq <- "BD,C"
  # ------------------------------------
  
 
  
  i <- gsub("^(.*?),.*", "\\1", qq)  # pull out before comma
  j <- sub('.*,\\s*', '', qq)        # pull out after comma
  a <-  dput(strsplit(i, "")[[1]])  
  b <- dput(strsplit(j, "")[[1]])  
 res1 <-   contrast(f, 
                  
                  list(IV= a), 
                  list(IV=b  ),   
                  conf.int=0.95,  #weights=table(df$IV),
                  type='average') 
 des.mat <-  print(res1, X=TRUE)
  
 # do again but weight according sample size is this necessasy VAR COVAR holds this info
 
 tt <- table(df$IV)               # get sample sizes of groups
 idx <- names(tt) %in% a          # identify TRUE FALSE the levels in the contrast
 w <- as.vector(tt)[idx]/sum(as.vector(tt)[idx])   # weights according to n in each level
 
 res2 <-   contrast(f, 
                    
                    list(IV= a), 
                    list(IV=b  ),   
                    conf.int=0.95,  #weights=table(df$IV),
                    type='average', weights=w)  
 des.mat <-  print(res2, X=TRUE)
 
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
   fit =f
   a =  list(IV=c("B", "D"))
   b =  list(IV='C')
 
   type = c(  "average") 
   conf.type = c("individual" )
 
   weights = "equal"
   conf.int = 0.95
   expand = TRUE
 
    
    # choosing critical value eithr t or z 
    zcrit <- if (length(idf <- fit$df.residual)) 
      qt((1 + conf.int)/2, idf) else qnorm((1 + conf.int)/2)
  
    betas <- coef(fit)  # al  model coefficients
     
    nrp <- num.intercepts(fit, "var")  # should be 1 for this simple case
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # design matrix
    pred <- function(d) {
          predict(fit, d, type = "x")
    }
    
    # left side
    da <- do.call("gendata", list(fit, factors = a, expand = expand))
    xa <- pred(da)    # will give a design matrix
 
    
    # right side
    db <- do.call("gendata", list(fit, factors = b, expand = expand))
    xb <- pred(db)     # will give a design matrix
      
    ma <- nrow(xa)
    mb <- nrow(xb)
    
    mm <- 2 #max(mall)

    # design matrix, removes labels
    xa <- matrix(xa, nrow = mm, ncol = ncol(xa), byrow = TRUE)
    xb <- matrix(xb, nrow = mm, ncol = ncol(xb), byrow = TRUE)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # design matrix difference
    X <- xa - xb  # subtract 
    X
    
    m <- nrow(X)
    
    # binding column of zeros
    if (nrp > 0)  X <- cbind(matrix(0, nrow = m, ncol = nrp), X)
    
    X  # this includes intercept
    
    weights <- rep(1, m)  ## 1 1
    weights <- as.vector(weights)  # 1 1
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    if (m > 1 && type == "average") 
      
      X <- matrix(apply(weights * X, 2, sum)/sum(weights), 
                  nrow = 1, dimnames = list(NULL, dimnames(X)[[2]]))
     
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~contrast
    
      X   # [1x4]  this is the weights!   #  0  0.5   -1  0.5
      
      # weight betas and sum
      est <- matxv(X, betas)   # contrast!!!!!!!!!!!!!!!!!!!!!!!!  #  X %*% betas
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~variance

      var.cov <- vcov(fit, regcoef.only = TRUE)   # 4X4
      
      var.cov
      
      
      v <- X %*% var.cov %*% t(X)  # variance!!!!!!!!!!!!!!!!!!!!!!!!  [1x4] x [4X4] x [4X1]
      
      ndf <- if (is.matrix(v))  nrow(v) else 1   #TRUE  #1
      
      se <- as.vector(if (ndf == 1) sqrt(v) else sqrt(diag(v))  )  
      
      Z <- est/se    # standard normal
      
      P <- if (length(idf)) 2 * pt(-abs(Z), idf)  else 2 * pnorm(-abs(Z))
      
      lower <- est - zcrit * se
      upper <- est + zcrit * se
  
    res <- list(Contrast = est, SE = se, Lower = lower, Upper = upper, 
                Z = Z, Pvalue = P,  
                var = v, 
                df.residual = idf, 
                X = X, 
                
               # cnames = if (type == "average") NULL else cnames, 
                conf.type = conf.type, conf.int = conf.int
                 )
  
  
   
  res
  
  contrast (f, list(IV=c("B","D")), list(IV="C"), type = "average") #
  contrast (f, list(IV=c("B","D")), list(IV="C"), type = "individual") 
  contrast (f, list(IV=c("B","D")), list(IV="C"), type = "joint") 
  
  
    
  
  
  
  
  
  
  
  
  
  
  
  
 ##################################### 
  
  
  
  
  
  
  
   