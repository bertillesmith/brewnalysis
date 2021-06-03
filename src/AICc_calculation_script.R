aic_func <- function(npar, nsamp, rss){
  aic_val <- 2*npar + nsamp*log(rss/nsamp)
  return(aic_val)
}  

aicc_func <- function(npar, nsamp, aic_val){
  if (nsamp <= npar+1){
    aicc_val <- NA
    return(aicc_val)
  } else{
    aicc_val <- aic_val + ((2*npar^2) + 2*npar)/(nsamp - npar - 1)
  }
  return(aicc_val)
}

delta_aicc_func <- function(min_aicc, aicc_val){
  delta_aicc <- aicc_val - min_aicc
  return(delta_aicc)
}

rmse_func <- function(obs, pred){
  rmse_val <- sqrt(mean((obs-pred)^2))
  return(rmse_val)
}