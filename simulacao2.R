
n.month=15
n.plot=5

rain <- sample(1:10,n.month,replace=T)
sand <- 1:n.plot

date<-rep(1:n.month,each=length(sand))

a <- log(0.002/(1-0.002))
b <- 0
c <- log(.1/(1-.1))
d <- 1

data<-data.frame(id=1:(length(rain)*length(sand)),plot=1:length(sand),date=date,rain=rain[date],sand=sand)
data$flor<-data$rain*0+20

data$flor<-rpois(n.month*n.plot,20)


#flor <- rpois(300,5)
#flor <- rep(50,)

fruto2fruto<-{0}

logis<-function(parms,x)exp(x%*%parms)/(1+exp(x%*%parms))

pfruto.fruto<-logis(c(a,b),cbind(1,data$rain))
pflor.fruto<-logis(c(c,d),cbind(1,data$rain))

plot(function(x){exp(a+b*x)/(1+exp(a+b*x))},ylim=c(0,1),xlim=c(1,10),col=4)
plot(function(x){exp(c+d*x)/(1+exp(c+d*x))},ylim=c(0,1),xlim=c(1,10),add=T,col=2)

points(pfruto.fruto~data$rain,ylim=c(0,1))
points(pflor.fruto~data$rain,ylim=c(0,1))

flor2fruto <- rbinom(data$flor,data$flor,pflor.fruto) 

points(flor2fruto/data$flor~data$rain,pch=21,bg=2)

past.fruto<-c(rep(0,sum(data$date==1)),flor2fruto[data$date!=max(data$date)])

fruto2fruto<-rbinom(past.fruto,past.fruto,pfruto.fruto[data$date!=max(data$date)])

points(fruto2fruto/past.fruto~data$rain,pch=21,bg=4)

plot(past.fruto~data$date)
plot(past.fruto~data$rain)

fruto <- flor2fruto + fruto2fruto

plot(data$flor~data$date,pch=21,bg="pink",xlim=c(0,10),ylim=c(0,40))
points(fruto~data$date,pch=21,bg="brown")
points((data$date-1),past.fruto,pch=21,bg="green")


###############

teste1<-data.frame(fruto,data$flor,past.fruto,data$rain,past.frutoT=c(fruto[data$date==1],fruto[data$date!=max(data$date)]))[data$date>4,]

###############

pFR.test<-seq(0,1,length=20)
pFL.test<-seq(0,1,length=20)

like.surf<-matrix(NA,length(pFR.test),length(pFL.test))

for (i in 1:length(pFR.test)){
for (j in 1:length(pFL.test)){

like.surf[i,j]<-NLL.flavio1(teste1$fruto,teste1$data.flor,teste1$past.fruto,pFR.test[i],pFL.test[j])

}}

image(like.surf,col=topo.colors(100),xlab="pFlower",ylab="pFruit")
contour(like.surf,add=T,nlevels=10)

image(exp(-like.surf),col=topo.colors(100),xlab="pFlower",ylab="pFruit")
contour(exp(-like.surf),add=T)

resu<-optim(par=list(pFR=0.5,pFL=0.5),function(x)NLL.flavio1(N=teste1$fruto,FL=teste1$data.flor,FR=teste1$past.fruto,x[1],x[2]))
points(resu$par[2],resu$par[1],pch=3,cex=1)


###########

resu2<-optim(par=list(a=0,b=0,c=0,d=0),
function(x)
NLL.flavio.logis(
N=teste1$fruto,
FR=teste1$past.frutoT,
FL=teste1$data.flor,
covar=teste1$data.rain,
parms=x))

rbind(resu2$par,c(a,b,c,d))

plot(logis(resu2$par[1:2],cbind(1,seq(0,10,length=100))),type="l",ylim=c(0,1))
points(logis(c(a,b),cbind(1,seq(0,10,length=100))),type="l",lty=2)

points(logis(resu2$par[3:4],cbind(1,seq(0,10,length=100))),type="l",ylim=c(0,1),col=2)
points(logis(c(c,d),cbind(1,seq(0,10,length=100))),type="l",col=2,lty=2)


NLL.flavio.logis(
N=teste1$fruto,
FL=teste1$data.flor,
FR=teste1$past.fruto,
covar=teste1$data.rain,
parms=c(a,b,c,d)
)

NLL.flavio.logis(
N=teste1$fruto,
FR=teste1$past.fruto,
FL=teste1$data.flor,
covar=teste1$data.rain,
parms=resu2$par
)

a.test<-seq(-5,5,length=100)
b.test<-seq(-5,5,length=100)

like.logis.surf1<-matrix(NA,length(a.test),length(b.test))

for (i in 1:length(a.test)){
for (j in 1:length(a.test)){

like.logis.surf1[i,j]<-NLL.flavio.logis(
N=teste1$fruto,
FL=teste1$data.flor,
FR=teste1$past.fruto,
covar=teste1$data.rain,
parms=c(a.test[i],b.test[j],c,d)
)

}}


image(like.logis.surf1,col=topo.colors(100),xlab="b",ylab="a")
contour(like.logis.surf1,add=T,nlevels=50)

image(exp(-like.logis.surf1),col=topo.colors(100),xlab="b",ylab="a")
contour(exp(-like.logis.surf1),add=T,nlevels=10)

z<-like.logis.surf1

persp3d(b.test,a.test,(like.logis.surf1),col=heat.colors(100)[cut(z,100)],xlab="b",ylab="a")

persp3d(b.test,a.test,exp(-z)*10^25,col=heat.colors(100)[cut(exp(-z),100)],xlab="b",ylab="a")



points(resu$par[2],resu$par[1],pch=3,cex=1)




##########

?contour


fruto
data$flor
past.fruto

flor2fruto


mean(rnorm(10,5,5))




plot(data$flor~data$rain)
plot(fruto~data$rain)


plot(fruto[-c(1:5)]~fruto[-c(46:50)])
plot(fruto[-c(46:50)]~data$rain[-c(1:5)])

summary(lm(fruto[-c(1:5)]~fruto[-c(46:50)]))
abline(lm(fruto[-c(1:5)]~fruto[-c(46:50)]))


######################################


LLbbinom

dnorm2<- function(x,w1=.5,w2=1-w1,m1=0,m2=3,sd1=1,sd2=1){
w1*dnorm(x,m1,sd1)+w2*dnorm(x,m2,sd2)
} 

dbinom2<- function(x,w1=.5,w2=1-w1,N1=10,N2=10,p1=.7,p2=.3,...){
w1*dbinom(x,N1,p1,...)+w2*dbinom(x,N2,p2,...)
} 

nLLbinom2<-function(x,w1=.5,w2=1-w1,N1=10,N2=N1,p1=.7,p2=.3)
-sum(dbinom2(x,w1=.5,w2=1-w1,N1=10,N2=10,p1=.7,p2=.3,log=T))

nLLflavio<-function(y,x,parms=c(a=0,b=0,c=0,d=0,w1=0),N1=10,N2=N1){
p1=exp(cbind(1,x)%*%parms[1:((length(parms)-1)/2)])/(1+exp(cbind(1,x)%*%parms[1:((length(parms)-1)/2)]))
p2=exp(cbind(1,x)%*%parms[(1+(length(parms)-1)/2):(length(parms)-1)])/(1+exp(cbind(1,x)%*%parms[(1+(length(parms)-1)/2):(length(parms)-1)]))
#w1=parms[length(parms)]
w1=.5

-sum(dbinom2(y,w1=w1,w2=1-w1,N1=N1,N2=N2,p1=p1,p2=p2,log=T))

}


y<-fruto[data$date!=min(date)]
x<-data$rain[data$date!=max(date)]
N1<-data$flor[data$date!=max(date)]
N2<-fruto[data$date!=max(date)]

nLLflavio(y,x,N1=N1*2,N2=N2*2)

optim(par=c(5,-0.5,-1,0.5,1),nLLflavio,y=y,x=x,N1=N1*2,N2=N2*2)

optim

prod(dbinom(15,10,.5))




### Debugging

nLLflavio(2:5,5,parms=c(0,0,0,0,1),N1=2:5)

-sum(log(dbinom(2:5,2:5,.5)))


fruto[]


nLLbinom2(5)
-log(dbinom2(5))
-log(dbinom(5,10,.5))


nLLbinom2(3)

nLLbinom2(rep(10,20))
nLLbinom2(rep(4,20))






plot(dbinom2(0:10),type="l")


.5*dbinom(1,1,.5)+.5*dbinom(1,1,.5)




















