#' Calculate Decision Curve Data
#'
#' @param ... one or more results of logistic or cox regression
#' @param model.names names for models
#' @param test.harm test harm, default is 0
#' @param times times for cox regresion, default is 'median'
#' @param new.data new data for validation
#'
#' @importFrom stats as.formula predict update median complete.cases
#' @importFrom rms Survival lrm
#' @return a dataframe contains thresholds, TPR: true positive rate, FPR: false
#'     positive rate, NB: net benefit, model: model names.
#' @export
#' @references Vickers, A. J., & Elkin, E. B. (2006). Decision Curve Analysis: A Novel Method for Evaluating Prediction Models. Medical Decision Making, 26(6), 565–574. https://doi.org/10.1177/0272989X06295361
dca <- function(...,
                model.names=do::get_names(...),
                test.harm=0,
                new.data=NULL,
                times='median'){
    fit.list<-list(...)
    model.names<-model.names
    times<-times
    test.harm<-test.harm
    new.data<-new.data
    check=sapply(fit.list, function(i) 'coxph' %in% class(i))
    fit.lrm=fit.list[!check]
    fit.cph=fit.list[check]
    if (length(fit.cph)>0){
        dca.cph(fit.list = fit.cph,
                model.names = model.names[check],
                test.harm = test.harm,
                times=times,
                new.data=new.data)
    }else if (length(fit.lrm)>0){
        dca.lrm(fit.list = fit.lrm,
                model.names = model.names[!check],
                test.harm = test.harm,
                new.data=new.data)
    }
}
