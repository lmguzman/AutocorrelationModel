#This script looks at the effect of temporal autocorrelation in the diversity of a community. This community is composed of 1 prey and any number of predators. The temporal autocorrelation acts on the growth rate of the prey. The community is a metacommunity with any number of patches, conected by global dispersal, where all the patches are on average sinks. 
#See the inflationary effect of temporal autocorrelation: Matthews and Andrew 2007

#Equations:
#  Nt+1'=R*exp(-Nt/K)*exp(-sum(a*Pt))
#  Pit+1'= ci*Nt*(1-exp(-ai*Pit))*exp(-sum(aj*Pjt),wj>wi)

#migration: Nt+1=(1-m)*Nt+1' + (m/L)*sum(Njt+1)    Same for the migration of the predators 
#Autocorrelation: Rt+1= exp(((rt+1)*std)+mean)
#rt+1=rho*rt+sqrt(1-rho^2)*u(t)

#The initial for loop goes through several combinations of attack rate (a), conversion rate(c), competition value (w) and migration rate (m) for the predators
#mclapply then goes through several autocorrelation values 
#Then with the function predpreymanypred using the same autocorrelation and a,c,w,and m values, the function runs any generations with any number of replicates. This function returns the final abundance of the prey and predators in a array of the dimesion= patch*replicates*species

#Predpreymanypred function:


#needs: prednum= number of predators, patch= number of patches, N0=initial population size of the prey, P0= initial population size of the predators,a= vector with the attack rate of all the predators (length is prednum), c=conversion rate of the predators (length is prednum), m1=migration rate of the prey, m2=migration rate of the predator (length is prednum), w= the competition coefficient, which ever predator has higher w wins when two predators infect the same prey,K= carrying capacity of the prey, rho=correlation coefficient, mn=mean and std=standard deviation of the growth rate, numruns= number of runs, gens=number of generations,t=treshold below which the population is 0, 

predpreymanypred<-function(prednum=2,patch=3,N0=10, P0=5,con=c(0.4,0.4), a=c(0.35,0.35), m1=0.1,m2=c(0.1,0.1),w=c(1,2), K=10^10, numruns=5, gens=10, mn=-0.1,t=10^-20,rho=0.5,std=0.7)
{
  library(abind)
  #creates an initial matrix and array to keep the prey and predator densities 
  allrand<-array(rnorm(patch*numruns*(gens+1),0,1), dim=c(patch,numruns,(gens+1)))
  ipops<-matrix(N0,nrow=patch,ncol=numruns)
  ippops<-array(P0, dim=c(patch,numruns,prednum))
  #initial noise, gets replaced every generation
  noise<-allrand[,,1]
  
  for(i.gens in 1:gens)
  {
    #within patch dynamics
    
    #Calculate the predator burden for the prey (that is *exp(-a*ppops)* for every predator)
    prev.pred<-sweep(ippops,MARGIN=3,a,FUN='*')
    pred.burden<-exp(-1*apply(prev.pred,MARGIN=c(1,2),FUN=sum))
    
    #Prey abundance Nt+1'=R*exp(-Nt/K)*exp(-sum(a*Pt))
    ipops.new<-exp((noise*std)+mn)*ipops*exp(-ipops/K)*pred.burden
    
    #All the predators abundance    Pit+1'= Nt*ci*(1-exp(-ai*Pit))*exp(-sum(aj*Pjt),wj>wi)
    #(1-exp(-ai*Pit))
    prev.pred1<-1-exp(-1*prev.pred)
    #ci*(1-exp(-ai*Pit))
    prev.pred2<-sweep(prev.pred1,MARGIN=3,con,FUN='*')
    #Nt*ci*(1-exp(-ai*Pit))
    prev.pred3<-apply(prev.pred2,MARGIN=3,FUN=function(x){x*ipops})
    prev.pred4<-array(prev.pred3,dim=c(patch,numruns,prednum))
    
    #exp(-sum(aj*Pjt),wj>wi)
    y<-1:prednum
    small.comp<-lapply(y,FUN=function(x){which(w[x]<w)})
    other.pred<-lapply(small.comp,FUN=function(x){apply(prev.pred[,,x],MARGIN=c(1,2),FUN=sum)})
    other.pred2<-array(unlist(other.pred),dim=c(patch,numruns,prednum))
    
    ippops.new<-prev.pred4*exp(-1*other.pred2)
    
    
    #threshold
    ipops.new[ipops.new<t]<-0
    ippops.new[ippops.new<t]<-0
    #migration
    cosum<-matrix(apply(X=ipops.new,MARGIN=2,FUN=sum),nrow=patch,ncol=numruns,byrow=T)
    ipops<-(1-m1)*ipops.new+(m1/(patch-1))*(cosum-ipops.new)
    
    #Predator migration
    pcosum<-aperm(array(colSums(ippops.new),dim=c(numruns,prednum,patch)),perm=c(3,1,2))
    pjsum<-pcosum-ippops.new
    m2rem<-(1-m2)
    pirem<-sweep(ippops.new,MARGIN=3,m2rem,FUN='*')
    m2arr<-m2/(patch-1)
    pjmig<-sweep(pjsum,MARGIN=3,m2arr,FUN='*')
    ippops<-pirem+pjmig
    
    #update noise
    noise<-rho*noise+sqrt(1-rho^2)*allrand[,,i.gens]	
    
  }
  
  total<-abind(ipops,ippops,along=3)
 
  return(total)
}