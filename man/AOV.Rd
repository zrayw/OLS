% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OLS.R
\name{AOV}
\alias{AOV}
\title{Anova Table}
\usage{
AOV(model)
}
\arguments{
\item{model}{a fitted model by OLS function.}
}
\value{
The function returns a matrix  of complete anova table for
the fitted model. It contains explained variance, f test and its corresponding
p value for each variable
}
\description{
Compute analysis of variance tables for model fitted by OLS .
}
\examples{
## Continue with OLS example
outcome = "mpg"
predictors = c("cyl", "hp", "wt")
m1 = OLS(mtcars, outcome, predictors, through_origin=FALSE)
AOV(m1)

}
