#' Area under Decision Curve
#'
#' @param x results of dca() function
#'
#' @return Area under decision curves for each model.
#' @importFrom set %not%
#' @export
#'
#' @examples
#' 1+1
AUDC <- function(x){
    x=x[x$thresholds != 0,]
    model=levels(x$model)
    for (j in 1:length(model)) {
        if (j==1) res=c()
        md.i=model[j]
        yy=x$NB[x$model==md.i]
        xx=x$thresholds[x$model==md.i]
        for (i in 1:(length(xx)-1)) {
            if (i==1) auc=0
            if (all(yy[i]>0,yy[i+1]>0)){
                high=xx[i+1]-xx[i]
                auc=auc+(yy[i]+yy[i+1])*high*1/2
            }
            if (all(yy[i]>0,yy[i+1]<0)){
                high=yy[i]
                x.mid=xx[i]-yy[i]*(xx[i+1]-xx[i])/(yy[i+1]-yy[i])
                auc=auc+(x.mid-xx[i])*high/2
            }
            if (all(yy[i]<0,yy[i+1]>0)){
                high=yy[i+1]
                x.mid=xx[i]-yy[i]*(xx[i+1]-xx[i])/(yy[i+1]-yy[i])
                auc=auc+(xx[i+1]-x.mid)*high/2
            }
            if (all(yy[i]>0,yy[i+1]==0)){
                high=yy[i]
                auc=auc+(xx[i+1]-xx[i])*high/2
            }
            if (all(yy[i]==0,yy[i+1]>0)){
                high=yy[i+1]
                auc=auc+(xx[i+1]-xx[i])*high/2
            }
        }
        res=c(res,auc)
    }
    names(res)=model
    res
}
