#'  Logistic population growth derivative with harvesting - fixed harvest 
#' @param time time since start
#' @param P population
#' @param parms - as list with three values, r, K, harv
#' @param r intrinsic growth rate 
#' @param K carrying capacity
#' @param h harvest rate
#' @return derivative of population with time 

dharvest_fixed= function(Time, P, parms) {
  
  min_C = 0
  
  if(parms$harv < min_C) {"error"}
  
  dP = parms$r * P * (1- P/parms$K) - parms$harv
  return(list(dP))
}
