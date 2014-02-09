
N<-c(6,6)

N<-6

FR=4
FL=2

pFL=0.4
pFR=0.6

NLL.flavio1<-function(N,FR,FL,pFR,pFL){

all.var<-data.frame(N,FR,FL,pFR,pFL)
for(a in names(all.var))assign(a,unlist(all.var[a]))

like.N<-{}

for(k in 1:length(N)){
if((FR[k]+FL[k])<N[k]){like.N[k]<-0}else{

combines<-expand.grid(0:FR[k],0:FL[k])
combs2N<-combines[rowSums(combines)==N[k],]
allcomb<-{}

for(i in 1:nrow(combs2N)){
allcomb[i]<-prod(dbinom(as.numeric(combs2N[i,]),c(FR[k],FL[k]),c(pFR[k],pFL[k])))
}
like.N[k]<-sum(allcomb)
}}
-sum(log(like.N))
}

#log(Like.flavio(N,FR,FL,pFR,.6))

NLL.flavio1(N,FR,FL,pFR,pFL)

pFR.test<-seq(0,1,length=50)
pFL.test<-seq(0,1,length=50)

like.surf<-matrix(NA,length(pFR.test),length(pFL.test))

for (i in 1:length(pFR.test)){
for (j in 1:length(pFL.test)){

like.surf[i,j]<-NLL.flavio1(c(22,16,16),16,6,pFR.test[i],pFL.test[j])

}}


NLL.flavio.logis<-function(N,FR,FL,covars=NULL,parms=rep(0,(length(covars)*2)+2)){

linear.pFR<-as.matrix(cbind(1,covars))%*%parms[1:(length(parms)/2)]
linear.pFL<-as.matrix(cbind(1,covars))%*%parms[(1+length(parms)/2):length(parms)]

pFR<-exp(linear.pFR)/(1+exp(linear.pFR))
pFL<-exp(linear.pFL)/(1+exp(linear.pFL))

#c(pFR,pFL)

NLL.flavio1(N,FR,FL,pFR,pFL)

}

NLL.flavio.logis(N,FR,FL,covars=data.frame(1:10))


NLL.flavio.logis(rep(N,10),FR,FL,covars=NULL)

NLL.flavio.logis(rep(N,1),FR,FL,covars=data.frame(1:10),parms=c(a=0,log(.6/(1-.6)),a2=0,log(.6/(1-.6))))

NLL.flavio1(N,FR,FL,0.6,0.6)





# plot the negative log likelihood (smaller values = higher likelihood)

image(like.surf,col=topo.colors(100),xlab="pFlower",ylab="pFruit")
contour(like.surf,add=T)

image(exp(-like.surf),col=topo.colors(100))
contour(exp(-like.surf),add=T)


persp(like.surf,zlim=c(0,57),expand=.8,col="lightblue", shade = 0.9,border=NA)

