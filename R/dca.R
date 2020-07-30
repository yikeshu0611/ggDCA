#' Calculate Decision Curve Data
#'
#' @param ... one or more results of logistic or cox regression
#' @param model.names names for models
#' @param include.cutoff logical. Whether the cutoff threshold should be included.
#' @importFrom stats as.formula predict update
#' @return
#' @export
#'
#' @examples
#' 1+1
dca <- function(...,
                model.names=do::get_names(...),
                include.cutoff=TRUE){
    fit.list<-list(...)
    df=lapply(1:length(fit.list), function(i) thresholds(fit=fit.list[[i]],
                                                    model.name = model.names[i],
                                                    include.cutoff=include.cutoff))
    res.threshold=do.call(rbind,df)
    # y is the same
    # get the most range for ref
    res.base = base(fit=fit.list[[1]],
                    thresholds=sort(unique(res.threshold$thresholds)))
    res=rbind(res.threshold,res.base)
    class(res)=c('ggdca','data.frame')
    res
}
thresholds <- function(fit,model.name=NULL,include.cutoff=TRUE) {
    if (is.null(model.name)) model.name=sub(' {2,}',' ',paste0(deparse(fit$terms)))
    if (all(c("lrm","rms","glm") %in% class(fit))){
        fitted=predict(fit,type = 'fitted')
    }else{
        fitted=fitted(fit)
    }
    thresholds=sort(unique(fitted))
    real=model.data(fit)[,model.y(fit)]
    # whatever 2 or more levels of y
    # we choose the biggest as 1
    # so it can be used for psm
    real=ifelse(real==max(real),1,0)
    # thresholds
    if (include.cutoff){
        TP=sapply(thresholds, function(x) table(paste0(ifelse(fitted>=x,1,0),real))['11'])
        FP=sapply(thresholds, function(x) table(paste0(ifelse(fitted>=x,1,0),real))['10'])
    }else{
        TP=sapply(thresholds, function(x) table(paste0(ifelse(fitted>x,1,0),real))['11'])
        FP=sapply(thresholds, function(x) table(paste0(pred=ifelse(fitted>x,1,0),real))['10'])
    }
    TP[is.na(TP)]=0
    FP[is.na(FP)]=0
    NB=TP/length(real)-FP/length(real)*thresholds/(1-thresholds)
    TPR=TP/length(real)
    FPR=FP/length(real)
    data.frame(thresholds,TPR,FPR,NB,model=model.name)
}
base <- function(fit,thresholds) {
    real=do::model.data(fit)[,do::model.y(fit)]
    real=ifelse(real==max(real),1,0)
    # ALL IS 1
    TP=rep(table(real)['1'],length(thresholds))
    FP=rep(table(real)['0'],length(thresholds))
    TP[is.na(TP)]=0
    FP[is.na(FP)]=0
    NB=TP/length(real)-FP/length(real)*thresholds/(1-thresholds)
    TPR=TP/length(real)
    FPR=FP/length(real)
    df1=data.frame(thresholds,TPR,FPR,NB,model='all')
    # ALL IS 0 is 0
    TPR=FPR=NB=rep(0,length(thresholds))
    df2=data.frame(thresholds,TPR,FPR,NB,model='none')
    rbind(df1,df2)
}
