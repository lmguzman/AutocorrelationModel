rm(list=ls())

setwd('/Users/meli_gu8/Documents/UNIVERSITY/UBC/AutocorrelationModel')

library('devtools')
library('lineprof')
library('microbenchmark')

source('FullModel.R')
l<-lineprof(predpreymanypred())
l
shine(l)

source('FullModel2.R')
l2<-lineprof(predpreymanypred2())
l2
shine(l2)

microbenchmark(predpreymanypred,predpreymanypred2)

library(pryr)

ev<-function(){
  a<-mem_used()
  b<-mem_change(predpreymanypred())
  c<-mem_change(predpreymanypred2())
  
  d<-system.time(predpreymanypred())
  e<-system.time(predpreymanypred2())
  return(c(a,b,c,d,e))
}

ev()

mem_used()
mem_change(predpreymanypred2())

system.time(predpreymanypred())
system.time(predpreymanypred2())
stopifnot(all.equal(sol1, sol2)) #to check that all the answers are equal to make sure that the new solution to the code is accurate too. 

microbenchmark(sol1, sol2)