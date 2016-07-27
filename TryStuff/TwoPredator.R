rm(list=ls())

#to create a function that will show predator-prey dynamics of two patches with migration

#returns the final population size of two patches after N runs 

#needs: N0, P0= initial population size, is a vector for two populations, m=migration rate, is constant and density independent
#K= carrying capacity, rho=correlation coefficient, mean and standard deviation of the growth rate, numruns= number of runs, t=treshold, 
#a= attack rate, c= conversion rate 


twopredator<-function(patch=100,N0=10, P0=5,c=0.4, a1=0.3,a2=0.2, m1=0.1,m2=0.1,m3=0.1, K=10^10, numruns=1000, gens=2000, mn=0, std=1, rho=0, t=10^-20)
{
  allrand<-array(rnorm(patch*numruns*(gens+1),0,1), dim=c(patch,numruns,(gens+1)))
  pops<-array(NA,dim=c(patch,numruns,3))
  pops.new<-array(NA,dim=c(patch,numruns,3))
  pops[,,1]<-N0
  pops[,,2]<-P0
  pops[,,3]<-P0
  noise<-allrand[,,1]
  for(i.gens in 1:gens)
  {
    #within patch dynamics
    pops.new[,,1]<-exp((noise*std)+mn)*pops[,,1]*exp(-pops[,,1]/K)*exp(-a1*pops[,,2])*exp(-a2*pops[,,3])
    pops.new[,,2]<-c*pops[,,1]*(1-exp(-a1*pops[,,2]))
    pops.new[,,3]<-c*pops[,,1]*(1-exp(-a2*pops[,,3]))
    #threshold
    pops.new[pops.new<t]<-0
    #migration
    cosum<-matrix(apply(X=pops.new[,,1],MARGIN=2,FUN=sum),nrow=patch,ncol=numruns,byrow=T)
    pcosum<-matrix(apply(X=pops.new[,,2],MARGIN=2,FUN=sum),nrow=patch,ncol=numruns,byrow=T)
    p2cosum<-matrix(apply(X=pops.new[,,3],MARGIN=2,FUN=sum),nrow=patch,ncol=numruns,byrow=T)
    
    pops[,,1]<-(1-m1)*pops.new[,,1]+(m1/(patch-1))*(cosum-pops.new[,,1])
    pops[,,2]<-(1-m2)*pops.new[,,2]+(m2/(patch-1))*(pcosum-pops.new[,,2])
    pops[,,3]<-(1-m3)*pops.new[,,3]+(m3/(patch-1))*(pcosum-pops.new[,,3])
    
    #update noise
    noise<-rho*noise+sqrt(1-rho^2)*allrand[,,i.gens]
  }
  numpatchdeadp1<-apply(X=pops[,,2], MARGIN=2,FUN=function(x){sum(x<t)})
  numpatchdeadp2<-apply(X=pops[,,3], MARGIN=2,FUN=function(x){sum(x<t)})
  rpts<-sum((numpatchdeadp1==numpatchdeadp2)&(numpatchdeadp1==patch))
  pp<-1-((sum(numpatchdeadp1==patch)+sum(numpatchdeadp2==patch)-(rpts))/length(numpatchdeadp1))
  allres<-pp
  return(allres)
}