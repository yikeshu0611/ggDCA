% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dca.R, R/dca.coxph.R, R/dca.cph.R, R/dca.glm.R,
%   R/dca.lrm.R
\name{dca}
\alias{dca}
\alias{dca.coxph}
\alias{dca.cph}
\alias{dca.glm}
\alias{dca.lrm}
\title{Calculate Decision Curve Data}
\usage{
dca(...)

\method{dca}{coxph}(
  ...,
  model.names = do::get_names(...),
  test.harm = 0,
  new.data = NULL,
  times = "median"
)

\method{dca}{cph}(
  ...,
  model.names = do::get_names(...),
  test.harm = 0,
  new.data = NULL,
  times = "median"
)

\method{dca}{glm}(..., model.names = do::get_names(...), test.harm = 0, new.data = NULL)

\method{dca}{lrm}(..., model.names = do::get_names(...), test.harm = 0, new.data = NULL)
}
\arguments{
\item{...}{one or more results of logistic or cox regression}

\item{model.names}{names for models}

\item{test.harm}{test harm, default is 0}

\item{new.data}{new data for validation}

\item{times}{times for cox regresion, default is 'median'}
}
\value{
a dataframe contains thresholds, TPR: true positive rate, FPR: false
positive rate, NB: net benefit, model: model names.
}
\description{
Calculate Decision Curve Data
}
\references{
Vickers, A. J., & Elkin, E. B. (2006). Decision Curve Analysis: A Novel Method for Evaluating Prediction Models. Medical Decision Making, 26(6), 565–574. https://doi.org/10.1177/0272989X06295361
}
