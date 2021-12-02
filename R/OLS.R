#'Ordinary Least Square Method
#'
#'OLS is used to fit linear models using ordinary least square method.
#'It can be used to carry out estimates and hypothesis tests for each parameter
#'in linear regression. A simple analysis of variance is also provided but can
#'be examined further by AOV function in this package.'
#'
#'@param data an object of data frame or list containing all variables and
#'observations in the model
#'@param outcome an vector specifying an outcome in the model. It should be a
#'string or an integer to indicate the location of outcome in data.
#'@param predictors an vector specifying predictors in the model. It should be
#'string or integers to indicate the location of outcome in data.
#'@param through_origin logical. If TRUE the model is forced to go through
#'origin. The default is FALSE.
#'
#'@return A list of results of fitted model. design_matrix: The design matrix used.
#'response: The response used. coefficients: a named vector of estimated coefficients.
#'residuals: The response minus fitted values. df: The degree of freedom of residuals.
#'through_origin: A logical indicator of whether the model pass the origin.
#'variance: A vector containing basic analysis of variance of fitted model,
#'including SSE, SSR, SST and R squared statistics. summary: Summary of the
#'model results, containing estimates, results of hypothesis test (p value) and
#'fitting results (R-square statistics).
#'
#'@examples
#'## Build-in data set mtcars in R
#'outcome = "mpg"
#'predictors = c("cyl", "hp", "wt")
#'m1 = OLS(mtcars, outcome, predictors, through_origin=FALSE)
#'m2 = OLS(mtcars, outcome, predictors, through_origin=TRUE) # omitting intercept
#'
#'
#'@export
#'

OLS = function(data, outcome, predictors, through_origin=FALSE){
  n = nrow(data)
  intercept = rep(1,n)
  if (through_origin) {
    X = data[, predictors]
  } else {
    X = cbind(intercept, data[, predictors])
  }
  X = as.matrix(X)
  p = ncol(X)
  Y = data[, outcome]
  beta = c(solve(t(X)%*%X)%*%t(X)%*%Y)
  Y_hat = X%*%beta
  residuals = Y-Y_hat
  df = n-p
  Y_bar = mean(Y, na.rm=TRUE)
  if (through_origin) {
    SST = sum(Y^2)
  } else { SST = sum((Y-Y_bar)^2) }
  SSE = sum(residuals^2)
  SSR = SST-SSE
  var_beta = diag(solve(t(X)%*%X))*c(SSE/df)
  se_beta = sqrt(var_beta)
  t_stat = c(beta/se_beta)
  p_t = c(2*( 1-pt(abs(t_stat), df) ))
  f_stat = SSR*(n-p)/SSE/(p-1)
  p_f = 1-pf(f_stat, p-1, n-p)
  R2 = SSR/SST
  R2_adj = 1-SSE*(n-1)/SST/(n-p)
  summary = cbind(Estimates=beta,
                  Std_error=se_beta,
                  t_stat=t_stat,
                  p_value=p_t)
  colnames(summary) = c("Estimates", "Std error", "t stat", "p value")
  model_var = c(SSE, SSR, SST, R2, R2_adj)
  names(model_var) = c("SSE", "SSR", "SST", "R Squared", "Adjusted R Squared")
  rt = list(design_matrix=X, response=Y, coefficients=beta, residuals=residuals,
            df.residuals=df, through_origin=through_origin,
            variance=model_var, summary=summary)
  return(rt)
}

#'Confidence Intervals for OLS model
#'
#'Computes confidence intervals for parameters in model fitted by OLS
#'function'
#'
#'@param model a fitted model by OLS function.
#'@param variables a vector specifying which parameters are to be given intervals.
#'The default indicator for intercept is Intercept.
#'@param level The confidence level required. The default level is 0.95.
#'
#'@return
#'A matrix with columns giving lower and upper confidence limits for each
#'selected parameter.
#'
#'@examples
#'## Continue with OLS example
#'outcome = "mpg"
#'predictors = c("cyl", "hp", "wt")
#'m1 = OLS(mtcars, outcome, predictors, through_origin=FALSE)
#'CI(m1, c("intercept", "cyl", "hp", "wt"), level=0.95)
#'CI(m1,"cyl",level=0.9)
#'
#'@export
#'

CI = function(model, variables, level=0.95){
  summary = as.data.frame(model$summary)
  summary = summary[variables, ]
  alpha = 1-level
  df = model$df.residuals
  quantile = qt(1-alpha/2, df)
  lower = summary$Estimates-quantile*summary$"Std error"
  upper = summary$Estimates+quantile*summary$"Std error"
  rt = cbind(summary$Estimates, lower, upper)
  l = paste(alpha/2*100, "%")
  u = paste((1-alpha/2)*100, "%")
  colnames(rt) = c("Estimates", l, u)
  rownames(rt) = variables
  return(rt)
}

#'Anova Table
#'
#'Compute analysis of variance tables for model fitted by OLS .
#'
#'@param model a fitted model by OLS function.
#'
#'@return
#'The function returns a matrix  of complete anova table for
#'the fitted model. It contains explained variance, f test and its corresponding
#'p value for each variable
#'
#'@examples
#'## Continue with OLS example
#'outcome = "mpg"
#'predictors = c("cyl", "hp", "wt")
#'m1 = OLS(mtcars, outcome, predictors, through_origin=FALSE)
#'AOV(m1)
#'
#'@export
#'

AOV = function(model){
  X = model$design_matrix
  Y = model$response
  through_origin = model$through_origin
  Y_bar = mean(Y, na.rm=TRUE)
  data = cbind(X,Y)
  predictors = colnames(X)
  n = nrow(X)
  p = ncol(X)
  df = n-p
  SSE = rep(0,p)
  for (i in 1:p){
    Z = X[,1:i]
    residuals = Y-Z%*%solve(t(Z)%*%Z)%*%t(Z)%*%Y
    SSE[i] = sum(residuals^2)
  }
  if (through_origin) {
    SST = sum(Y^2)
  } else {
    SST = sum((Y-Y_bar)^2)
  }
  SS = c(SST-SSE[1], SSE[1:p-1]-SSE[2:p])
  f_stat=SS*df/SSE[p]
  p_f = c(1-pf(f_stat, 1, df))
  aov = cbind(rep(1, length(SS)), SS, f_stat, p_f)
  rownames(aov) = predictors
  aov = rbind(aov, Residuals=c(df, SSE[p], NA, NA))
  colnames(aov) = c("Df", "Sum of Squares", "f stat", "p value")
  return(aov)
}
