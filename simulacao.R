
head(flor)

head(flor.long)

head(flor.short)


nflor.parcela<-tapply(flor.short$x,flor.short$Group.1,sum)

ambiente[as.numeric(flor.short$Group.1),]

plot(nflor.parcela~ambiente$Sand)

fl.par.pois<-glm(nflor.parcela~Sand,family=poisson(),data=ambiente)
lines(seq(0,110,by=0.1),exp(cbind(1,seq(0,110,by=0.1))%*%fl.par.pois$coef))

plot(nflor.parcela~ambiente$Sand,log="y")
lines(seq(0,110,by=0.1),exp(cbind(1,seq(0,110,by=0.1))%*%fl.par.pois$coef))

sefit<-predict(fl.par.pois,newdata=list(Sand=seq(0,110,by=.1)),se.fit=TRUE,type="response")$se.fit
fit<-predict(fl.par.pois,newdata=list(Sand=seq(0,110,by=.1)),se.fit=TRUE,type="response")$fit

lines(seq(0,110,by=0.1),sefit*1.96+fit)
lines(seq(0,110,by=0.1),-sefit*1.96+fit)







summary(fl.par.pois)

########################


nfruto.parcela<-tapply(fruto.short$x,fruto.short$Group.1,sum)

ambiente[as.numeric(fruto.short$Group.1),]

plot(nfruto.parcela~ambiente$Sand)

fr.par.pois<-glm(nfruto.parcela~ambiente$Sand,family="poisson")
lines(seq(0,110,by=0.1),exp(cbind(1,seq(0,110,by=0.1))%*%fr.par.pois$coef))

plot(nfruto.parcela~ambiente$Sand,log="y")
lines(seq(0,110,by=0.1),exp(cbind(1,seq(0,110,by=0.1))%*%fr.par.pois$coef))

summary(fr.par.pois)








abund <- rep(10,30)

clay<-seq(0,1,length=30)

flpi<-sapply(1:30,function(x)rpois(abund[x],exp(clay[x])))

flpp<-colMeans(flpi)

probflpp<-colSums(flpi>0)/abund

plot(flpp~clay)
points(clay,predict(glm(flpp~0+clay,family=poisson),type="response"),type="l")

plot(probflpp~clay,ylim=c(0,1))
points(clay,predict(glm(probflpp~0+clay,family=binomial),type="response"),type="l")

#######

alpha=0.5

FL1=0

FLi<-matrix(rep(FL1,30),30,20)

for(i in 2:20){

FLi[,i] <- flpp-alpha*(FLi[,(i-1)])-(1-alpha)*(FLi[,(i-1)])

}


FLi











