#' Calculate Decision Curve Data
#'
#' @param ... one or more results of logistic or cox regression
#' @param model.names names for models
#' @param test.harm test harm, default is 0
#' @param times times for cox regresion, default is 'median'
#' @param new.data new data for validation
#' @return a dataframe contains thresholds, TPR: true positive rate, FPR: false
#'     positive rate, NB: net benefit, model: model names.
#' @references Vickers, A. J., & Elkin, E. B. (2006). Decision Curve Analysis: A Novel Method for Evaluating Prediction Models. Medical Decision Making, 26(6), 565–574. https://doi.org/10.1177/0272989X06295361
#' @export
#'
dca <- function(...) UseMethod('dca')

