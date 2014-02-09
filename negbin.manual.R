
#Create a negative binomial function to run the data (for some reason glm doesn't work with this data)

nLLnbin<-function(y,x,parms=c(log(mean(y)),0,1)){
a<-parms[1]
b<-parms[2]
size<-parms[3]
mu<- exp(a+b*x) 
-sum(log(dnbinom(y,size=size,mu=mu)))
#-sum(log((gamma(size+y)/(gamma(size)*factorial(y)))*(size/(size+mu))^size*(mu/(mu+size))^y))
}


dnbinom(5,size=2,mu=3)

(gamma(size+y)/(gamma(size)*factorial(y)))*(size/(size+mu))^size*(mu/(mu+size))^y


factorial(5+2-1)/(factorial(2-1)*factorial(5))*


LLnbin(nflor.parcela,ambiente$Sand)


nLLpois<-function(y,x,parms=c(log(mean(y)),0)){
a<-parms[1]
b<-parms[2]
mu<- exp(a+b*x) 
-sum(log(dpois(y,lambda=mu)))
}

nLLpois(nflor.parcela,ambiente$Sand)

opt.nb<-optim(par=c(1,1,1),fn=nLLnbin,y=nflor.parcela,x=ambiente$Sand)
opt.pois<-optim(par=c(0,0),fn=nLLpois,y=nflor.parcela,x=ambiente$Sand)

coefs<-opt.nb$par
coefs.pois<-opt.pois$par

plot(nflor.parcela~ambiente$Sand)

lines(spline(seq(0,110,by=1),exp(coefs[1]+coefs[2]*seq(0,110,by=1))))
lines(spline(seq(0,110,by=5),qnbinom(0.975,size=coefs[3],mu=exp(coefs[1]+coefs[2]*seq(0,110,by=5)))))
lines(spline(seq(0,110,by=5),qnbinom(0.025,size=coefs[3],mu=exp(coefs[1]+coefs[2]*seq(0,110,by=5)))))

lines(seq(0,110,by=0.01),exp(coefs.pois[1]+coefs.pois[2]*seq(0,110,by=0.01)),col=2)
lines(seq(0,110,by=0.01),qpois(0.975,exp(coefs.pois[1]+coefs.pois[2]*seq(0,110,by=0.01))),col=2)
lines(seq(0,110,by=0.01),qpois(0.025,exp(coefs.pois[1]+coefs.pois[2]*seq(0,110,by=0.01))),col=2)


AIC.nb <- -2*(-opt.nb$value)+2*3
AIC.pois <- -2*(-opt.pois$value)+2*2

AIC(fl.par.pois)

summary(fl.par.pois)

#Negative binomial fits the data much better than poisson



