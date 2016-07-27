library(parallel)
library(dplyr)
source('FullModel.R')
source('Summarizing.R')


run_model_pp_1p <- function(params){
  
  autocorrelation <- params[1]
  
  standard_deviation <- params[2]
  
  N_predators <- 1
  
  Runs <- 1000
  
  Generations <- 20000
  
  Int1 <- predpreymanypred(prednum = N_predators, con = 0.4, a = 0.35, m2 = 0.1, w = 1, rho = autocorrelation, 
                           std = standard_deviation, numruns = Runs, gens = Generations)
  
  Probability_persistence(Int1)
}

#Parameters of autocorrelation and standard deviation

allrho <- seq(0, 0.9, 0.1)
allstd <- seq(0, 0.9, 0.1)

params <- expand.grid(allrho, allstd) %>% t() %>% as.data.frame()

res<-mclapply(X = params, FUN = run_model_pp_1p, mc.cores = 6)

fin_res <- unlist(res) %>% data.frame(PP = ., expand.grid(allrho, allstd)) %>% 
  rename(Rho = Var1, Std = Var2)

saveRDS(fin_res, 'results_one_predator.rds')
