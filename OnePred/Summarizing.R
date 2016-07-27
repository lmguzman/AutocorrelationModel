#Input is an array, dim1 = patches, dim2 = runs, dim3 = species

Probability_persistence <- function(array1){
  
  t <- 10^-20
  patch <- dim(array1)[1]
  num_patch_death <- matrix(NA, nrow = dim(array1)[3], ncol = dim(array1)[2])
  
  for(i in 1:dim(array1)[3]){
    num_patch_death[i,] <- apply(X=array1[,,i], MARGIN=2,FUN=function(x){sum(x<t)})
  }
  
  #IF a run has any dead species, whole run is considered extinct 
  probability_persistece <- 1 - (sum(apply(num_patch_death, MARGIN = 2, FUN = function(x){any(x == patch)})) / ncol(num_patch_death))
  return(probability_persistece)
}



