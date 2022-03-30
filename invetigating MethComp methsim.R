# inputs
# unpacking meth.sim! better ubderstanding and figure out why BA plot in app has different stats!
# use meth.sim to simulate paired ttet
  require(MethComp)
  set.seed=101
  n = Ni = 10
  res <-   c(10,5)        # residual
  btw <-   c(10,3)  
  a <-     c(0.1,0.2)     # diff
  b <-     c(1.1, 0.9 )   # is it constant
  range <- c(0,10)
  ir <-1  
  
  # sliderInput("range1", 
  #             "Select group sizes: randomly select using range or uniquely select:", 
  #             min = 2, max = 5000, value = c(1000, 1000), ticks=FALSE) 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
  Nm = 2 # methods , fix this
  Nr = 3# reps pre sample , fix this
  nr = Nr
  alpha = a #rep(a, Nm)
  beta =  b #rep(b, Nm)
  mu.range =range    #  this will influence mean, the middle of this and correlation
  sigma.mi = btw #rep(btw,Nm)
  sigma.ir = ir
  sigma.mir = res#  rep(res, Nm)
   
  
  meth <- rep(1:Nm, Ni)   # methods 1:2 
  item <- rep(1:Ni, each = Nm)  ## 1 1 2 2 # samples 
  reps <- rep(Nr, length(meth))   # all 1s here
    
  dfr <- data.frame(meth = meth, item = item)[rep(1:length(meth), reps), ]
  dfr <- MethComp::make.repl(dfr)    # all 1
  
  meth <- dfr$meth
  item <- dfr$item
  repl <- dfr$repl

  # now we have the structure
  
  mu <- runif(Ni, mu.range[1], mu.range[2])  # true means of samples
  mu <- mu[item]
  

  # this will be 0
  # Item x replicate, Bendix says nothing to do with methods
  e.ir <- rnorm(nlevels(IR <- interaction(item, repl)), mean = 0, sd = sigma.ir)
  e.ir <- e.ir[as.integer(IR)]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e.mi <- rnorm(nlevels(MI <- interaction(meth, item)), mean = 0, sd = sigma.mi)
  e.mi <- e.mi[as.integer(MI)]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e.mir <- rnorm(nrow(dfr), mean = 0, sd = sigma.mir[meth])
  
  
  betavec <- beta[meth]
  alphavec <- alpha[meth]
  
  y <- alphavec + betavec * (mu + e.mi + e.ir) + e.mir
  
  # this is all that is needed
  dfr1 <- dfr <- data.frame(dfr, y = y)
  
  d1 <- Meth(dfr)
  
  # analysis
  if (Nr ==1) (BA.plot(d1, eqn=TRUE)) 
  if (Nr >1 & sigma.ir >0 ) BA.est(d1, linked=TRUE)
  if (Nr >1 & sigma.ir ==0 ) BA.est(d1, linked=FALSE)
  
  
  # lets get a dataset with true sds etc
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # shifts from mu and true parametrer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  dfr$mu <- mu
  dfr$IR <- e.ir
  dfr$MI <- e.mi
  dfr$MIR <- e.mir

  
  dfr$alpha <- alphavec
  dfr$beta <- betavec
  
  
  # put variance components in dfr too!
  
  dfr$ sigma.ir  <- sigma.ir 
  dfr$ sigma.mi  <-  sigma.mi[meth]
  dfr$ sigma.mir <-  sigma.mir[meth]

  
  #dfr <- data.frame(dfr, y = y)
  
  row.names(dfr) <- NULL
  head(dfr)
 
  
  # we have the data frame!
  
 
  r1 <- dfr1  
  
  IR <- interaction(item, repl) 
  MI <- interaction(meth, item)
  
  # r primer bo0k p144 edition 1
  library(lme4)
  lmer(y~ item + meth + (1|MI) + (1|IR) )
  
  lmer(y ~ item + (1|meth) + (1|MI) + (1|IR))
  
  # r primer edition 2
  # need more than 2 meths and more than 2 reps?
  result2 <- BA.est(d1, linked=TRUE, random.raters=TRUE )
  
  
  
  # A <- r1[r1$meth %in% 1,"y"]
  # B <- r1[r1$meth %in% 2,"y"]
  #  
  # d <- data.frame(y = A , x = B)
  # 
  # my_data <- data.frame( 
  #   group = rep(c("before", "after"), each = n),
  #   counts = c(d[,1],  d[,2]),
  #   ID=rep(1:n,2)
  # )
  # 
  # require(MethComp)
  # m <- Meth(my_data,meth="group",item="ID",repl=NULL,y="counts",print=TRUE)
  # 
  # L <- max(abs(floor(min(A-B))),abs(ceiling(max(A-B))))
  # 
  # 
  # roundUp <- function(x,to=10)
  # {
  #   to*(x%/%to + as.logical(x%%to))
  # }
  # 
  # L <- roundUp(L)
  # 
  # # we set up const dif and const sd so that will matches true data genrting mechanism
  # par( mfrow=c(1,1), mar=c(3,3,1,3), mgp=c(3,1,0)/1.6 )
  # 
  # BA.plot( m, model=NULL, repl.conn=TRUE, col.lines="blue",
  #          diflim=c(-L,L), 
  #          xaxs="i", yaxs="i",
  #          las=1, eqn=TRUE,  
  #          pl.type="BA", 
  #          sd.type="cons", 
  #          dif.type =  "cons",
  #          grid=1:9*10, digits=3,font.eqn=1  , eqax=T)
  # 
  # 
  # analysis repeat
  if (Nr ==1) (BA.plot(d1, eqn=TRUE))
  if (Nr >1 & sigma.ir >0 ) BA.est(d1, linked=TRUE)
  if (Nr >1 & sigma.ir ==0 ) BA.est(d1, linked=FALSE)

  if (Nr ==1) zz = NULL
  if (Nr >1 & sigma.ir >0 ) zz ="linked"
  if (Nr >1 & sigma.ir ==0 ) zz ="exch"
  
  
  BA.plot( r1, model=zz, repl.conn=TRUE, col.lines="blue",
           #diflim=c(-L,L), 
           xaxs="i", yaxs="i",
           las=1, eqn=TRUE,  
           pl.type="BA", 
           sd.type="cons", 
           dif.type =  "cons",
           grid=1:9*10, digits=3,font.eqn=1  , eqax=T)
  
  
  
  
 d <- dfr
 
 d$meth <- as.numeric(as.character (d$meth))
 
 d$xj <- jitter(d$meth, amount=.13)
 d$x <- d$meth 
 d$id <- d$item 
 
 # code from count app
 require(ggplot2)
 AA <- ggplot(data=d, aes(y=y) ) +
   geom_boxplot(aes(x=x, group=x), width=0.2, outlier.shape = NA, col='blue', fill='lightblue') +
   geom_line(aes(x=xj, group=id),  colour='pink', alpha=.5) +
   geom_point(aes(x=xj), size=2) +
   xlab("Phase") + ylab("Count") +  
   scale_x_continuous(breaks=c(1,2), labels=c("Before", "After"), limits=c(0.5, 2.5)) +
   # scale_y_continuous(breaks=c(mL:m), limits=c(mL,m)) +
   theme_bw() + theme(legend.position = "none") 
 
   print(AA)
  
  
  
  Aa<- unique(sigma.mi) ^2 + unique(sigma.mir)^2
  Ba<- unique(sigma.mi)^2 + unique(sigma.mir)^2
  sqrt(Aa+Ba)
  cor(A,B) 
  t.test(A,B, paired=T)
  
  