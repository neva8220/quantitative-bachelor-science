rm(list=ls())                       
library(rethinking)
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-9.0.1')
#library(xlsx)
#data <- data.frame(read.xlsx(file="C:\\Users\\user\\Desktop\\QBS\\data.xlsx",sheetIndex=2,startRow = 1,endRow=172,header = T))

data <- data.frame(read.xlsx(file='data.xlsx',sheetIndex=1,startRow = 1,endRow=172,header = T))
d <- data
d$e.s <- (data$education-mean(data$education))/sd(data$education)
d$b.s <- (data$burn.r-mean(data$burn.r))/sd(data$burn.r)
d$ec.s <- (data$economic-mean(data$economic))/sd(data$economic)

#跟下面model(二次的)比較的話的確負向效果有明顯一點
t1<- map(
  alist(
    crime.r ~ dnorm( mu , sigma ) ,
    mu <- a + b1*b.s ,
    a ~ dnorm( 4.775 , 1) ,
    b1 ~ dnorm( -0.3134 , 1 ) ,
    sigma ~ dunif( 0 , 2)
  ) ,
  data=d )
precis(t1, corr=TRUE)

plot( crime.r ~ burn.r ,data=d , col=col.alpha(rangi2,0.5) )
burn.seq <- seq(from=-2, to=4, length.out=50)
pd <- data.frame(b.s=burn.seq)
m <- link( t1 , data=pd )
m.mean <- apply( m , 2 , mean )
m.HPDI <- apply( m , 2 , HPDI , prob=0.89 )
lines( burn.seq*sd(data$burn.r)+mean(data$burn.r) , m.mean )
shade( m.HPDI , burn.seq*sd(data$burn.r)+mean(data$burn.r) )

##2次曲線(worse than m since a bigger)
d$b.s2 <- d$b.s^2
t2 <- map(
  alist(
    crime.r ~ dnorm( mu , sigma ) ,
    mu <- a + b1*b.s+ b2*b.s2 ,
    a ~ dnorm( 4.927 , 1 ) ,
    b1 ~ dnorm( -0.1993 , 1 ) ,
    b2 ~ dnorm( -0.1530 , 1 ) ,
    sigma ~ dunif( 0 , 2 )
  ) ,
  data=d )
precis(t2)

plot( crime.r ~ b.s ,data=d , col=col.alpha(rangi2,0.5) )
burn.seq <- seq(from=-2, to= 4, length.out=50)
pd <- data.frame(b.s=burn.seq, b.s2=burn.seq^2)
m <- link( t2 , data=pd )
m.mean <- apply( m , 2 , mean )
m.HPDI <- apply( m , 2 , HPDI , prob=0.89 )
lines( burn.seq , m.mean )
shade( m.HPDI , burn.seq )

compare(t1, t2)

#run some test model to find the best

t3<- map(
  alist(
    crime.r ~ dnorm( mu , sigma ) ,
    mu <- a + b1*b.s+b2*e.s ,
    a ~ dnorm( 4.775 , 1) ,
    b1 ~ dnorm( -0.28762 , 1 ) ,
    b2 ~ dnorm( 0.04719 ,1 ) ,
    sigma ~ dcauchy( 0 , 2)
  ) ,
  data=d )
t4<- map(
  alist(
    crime.r ~ dnorm( mu , sigma ) ,
    mu <- a + b1*b.s+b2*ec.s ,
    a ~ dnorm( 4.7750 , 1) ,
    b1 ~ dnorm( -0.33528 , 1 ) ,
    b2 ~ dnorm( -0.07189 ,1 ) ,
    sigma ~ dcauchy( 0 , 2)
  ) ,
  data=d )
t5<- map(
  alist(
    crime.r ~ dnorm( mu , sigma ) ,
    mu <- a + b1*b.s+b2*ec.s+b3*e.s ,
    a ~ dnorm( 4.7750 , 1) ,
    b1 ~ dnorm( -0.2097 , 1 ) ,
    b2 ~ dnorm( -0.3582 , 1 ) ,
    b3 ~ dnorm( 0.3887 , 1) ,
    sigma ~ dcauchy( 0 , 2)
  ) ,
  data=d )
t6<- map(
  alist(
    crime.r ~ dnorm( mu , sigma ) ,
    mu <- a + b1*b.s+b2*ec.s+b3*ec.s*b.s ,
    a ~ dnorm( 4.89784 , 1) ,
    b1 ~ dnorm( -0.29673 , 1 ) ,
    b2 ~ dnorm( 0.02945 , 1 ) ,
    b3 ~dnorm( 0.40648, 1 ) ,
    sigma ~ dcauchy( 0 , 2)
  ) ,
  data=d )
t7 <- map(
  alist(
    crime.r ~ dnorm( mu , sigma ) ,
    mu <- a + b1*b.s+b2*e.s+b3*e.s*b.s ,
    a ~ dnorm( 4.959 , 1 ) ,
    b1 ~ dnorm( -0.1709 , 1) ,
    b2 ~ dnorm( 0.1756 , 1 ) ,
    b3 ~dnorm( 0.3384 , 1 ) ,
    sigma ~dcauchy( 0 , 2)
  ) ,
  data=d )
compare(t1, t2, t3, t4, t5, t6, t7)##exclude the last poor perform model t5, t3, t4, t2, t1

t6stan <- map2stan( t6 , data=d , iter=1e4 , warmup=1000 )
t7stan <- map2stan( t7 , data=d , iter=1e4 , warmup=1000 )

compare(t6stan, t7stan)##the best t6stan
#so education and economic 
#may have multicollnearity so they can't be put as variable together
#as they make a worse model
##with education interaction (t6) there is a better model
###with crime norm distribution (t7) more better model
###since t6 is the best model and since i ignore some of  
###the behind variable so there is no use to draw a 
###predict plot.Thus, we use counterfactual plot

##plot counterfactual 
###1st(ec.s) =>much more negative
plot( crime.r ~ burn.r , data=d ,
      xlab="fertility rate" ,
      ylab = "crime rate",
      col= ifelse(d$ec.s<-0.6912,col.alpha(rangi2,0.5),"white"))
mtext( "ecinomic situation below 25%" , 3 )
burn.seq <- seq(from=-2, to= 4, length.out=50)
pd <- data.frame(ec_s=-1.5553, b_s=burn.seq)
m <- link( t6stan, data=pd )
m.mean <- apply( m , 2 , mean )
m.HPDI <- apply( m , 2 , HPDI , prob=0.89 )
lines( burn.seq*sd(d$burn.r)+mean(d$burn.r), m.mean )
shade( m.HPDI , burn.seq*sd(d$burn.r)+mean(d$burn.r) )

##mean(ec.s) =>negative
plot( crime.r ~ burn.r , data=d , 
      xlab="fertility rate" ,
      ylab = "crime rate",
      col= ifelse(d$ec.s<0.2636 & d$ec.s>-0.6912,col.alpha(rangi2,0.5),"white"))
mtext( "ecinomic situation between 25% to 75%" , 3 )
burn.seq <- seq(from=-2, to= 4, length.out=50)
pd <- data.frame(ec_s=0, b_s=burn.seq)
m <- link( t6stan, data=pd )
m.mean <- apply( m , 2 , mean )
m.HPDI <- apply( m , 2 , HPDI , prob=0.89 )
lines( burn.seq*sd(d$burn.r)+mean(d$burn.r) , m.mean )
shade( m.HPDI , burn.seq*sd(d$burn.r)+mean(d$burn.r) )

##3rd(ec.s) =>flat
plot( crime.r ~ burn.r , data=d , 
      xlab="fertility rate" ,
      ylab = "crime rate",
      col= ifelse(d$ec.s>0.2636,col.alpha(rangi2,0.5),"white"))
mtext( "ecinomic situation above 75%" , 3 )
burn.seq <- seq(from=-2, to= 4, length.out=50)
pd <- data.frame(ec_s=0.2636, b_s=burn.seq)
m <- link( t6stan, data=pd )
m.mean <- apply( m , 2 , mean )
m.HPDI <- apply( m , 2 , HPDI , prob=0.89 )
lines( burn.seq*sd(d$burn.r)+mean(d$burn.r) , m.mean )
shade( m.HPDI , burn.seq*sd(d$burn.r)+mean(d$burn.r) )

##interesting discover
plot( crime.r ~ burn.r , data=d , 
      xlab="fertility rate" ,
      ylab = "crime rate",
      col= ifelse(d$ec.s>0.2636,col.alpha(rangi2,0.5),"white"))
mtext( "ecinomic situation with highest value" , 3 )
burn.seq <- seq(from=-2, to= 4, length.out=50)
pd <- data.frame(ec_s=3.5167, b_s=burn.seq)
m <- link( t6stan, data=pd )
m.mean <- apply( m , 2 , mean )
m.HPDI <- apply( m , 2 , HPDI , prob=0.89 )
lines( burn.seq*sd(d$burn.r)+mean(d$burn.r) , m.mean )
shade( m.HPDI , burn.seq*sd(d$burn.r)+mean(d$burn.r) )
