library(ggplot2)
library(data.table)

## m/s = z: parametrizing in terms of z score as natural units for m
zs <- seq(from=-5,to=5,by=0.1)
ss <- seq(from=0.1,to=5,by=0.1)

## check: taking pnorm = cumulative normal
f <- function(z,s){
  s^2 * dnorm(0,s*z,s) - s*z*(1-pnorm(-z))
}


D <- as.data.table(expand.grid(zs,ss))
names(D) <- c('z','s')
D[,F:=f(z,s)]

D

ggplot(D,aes(x=z,y=s,col=F))+
  geom_tile()

ggplot(D,aes(x=z,y=s,col=F, fill = F))+
  geom_tile()

ggplot(D[s==1],aes(z,F))+geom_line()

## maybe most useful:
ggplot(D,aes(z,F,col=s,group=s))+
  geom_line()+
  geom_vline(xintercept=0.5,col=2)



ggplot(D[z==1],aes(s,F))+geom_line()
