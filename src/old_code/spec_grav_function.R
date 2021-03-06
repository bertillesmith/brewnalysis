library("fBasics")
library("dfoptim")

# predicted values for the logistic function
log_func <- function(log_par, tvec){
  x0 <- log_par[1] #
  L <- log_par[2] #a
  k <- log_par[3]
  log_pred <- L/(1+exp(-k*(tvec-x0)))
}

# predicted values for the exponential (decay) function
exp_func <- function(exp_par, tvec){
  n0 <- exp_par[1]
  lambda <- exp_par[2]
  fg <- exp_par[3] #final gravity of fermentation
  exp_pred <- n0 * exp(-lambda*tvec)
}

exp_func_2 <- function(exp_par, fg, tvec){
  n0 <- exp_par[1]
  lambda <- exp_par[2]
  #fg is the final gravity of fermentation
  exp_pred <- fg + (n0 * exp(-lambda*tvec))
}

#functions to calculate the sums of squares

log_dev_sq <- function(vars, tvec, obs){
  y_pred <- log_func(vars, tvec)
  dev_sq <- (y_pred-obs)^2
  out <- sum(dev_sq)
  return(out)
}

exp_dev_sq <- function(vars, tvec, obs){
  y_pred <- exp_func(vars, tvec)
  dev_sq <- (y_pred-obs)^2
  out <- sum(dev_sq)
  return(out)
}

exp_dev_sq2 <- function(vars, fg, tvec, obs){
  y_pred <- exp_func_2(vars, fg, tvec)
  dev_sq <- (y_pred-obs)^2
  out <- sum(dev_sq)
  return(out)
}