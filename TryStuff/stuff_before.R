
#trial= number of a,c,m2,w combinations to be tested.
#trial<-10
#predators<-20
#crange<-0.4
#arange<-c(0.2,0.4)
#mrange<-0.1
#wrange<-c(0,100)

library(abind)
library(vegan)
#for(i in 1:trial)
#{
#cpre<-rep(crange,predators)
#apre<-runif(predators,arange[1],arange[2])
#m2pre<-rep(mrange,predators)
#wpre<-runif(predators,wrange[1],wrange[2])

#allrho<-seq(0,0.9,0.3)

#remember to activate the multicore package on the package tab

#library(parallel)
#res<-mclapply(X=allrho,FUN=function(x){predpreymanypred(prednum=predators,rho=x,a=apre,con=cpre,m2=m2pre,w=wpre)},mc.cores=5)

#resu<-paste('results',i,'.RData',sep='')
#save(res,file=resu)
#ran<-cbind(cpre,apre,m2pre,wpre)
#ran2<-paste('random',i,'.RData',sep='')
#save(ran,file=ran2)

#}


