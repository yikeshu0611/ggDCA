#' Calculate reduction in false positive count
#'
#' @param x result of dca() function
#'
#' @return a dataframe contains thresholds, rFP.100: reduction
#'     in false positive count per 100 patients.
#' @export
#'
rFP.p100 <- function(x){
    model.all = levels(x$model)
    for(i in 1:length(model.all)){
        if (i==1) df=NULL
        model=model.all[i]
        thresholds = (x$thresholds)[x$model==model.all]
        NB.medel=(x$NB)[x$model==model.all]
        NB.all=(x$NB)[x$model=='All' & x$thresholds %in% thresholds]
        advantage=NB.medel-NB.all
        rFP.p100=advantage*100/(thresholds/(1-thresholds))
        dfi=data.frame(thresholds,rFP.p100,model=model)
        df=rbind(df,dfi)
    }
    class(df)=c('rFP.p100','data.frame')
    df
}
