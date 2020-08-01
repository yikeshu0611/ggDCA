
dca.lrm <- function(fit.list,
                model.names=NULL,
                test.harm=0){
    if (length(fit.list)==0) return(NULL)
    df=lapply(1:length(fit.list), function(i) thresholds.lrm(fit=fit.list[[i]],
                                                    model.name = model.names[i]))
    res.threshold=do.call(rbind,df)
    res.threshold$NB=res.threshold$NB-test.harm
    # y is the same
    # get the most range for ref
    res.base = base.lrm(fit=fit.list[[1]],
                    thresholds=sort(unique(res.threshold$thresholds)))
    res=rbind(res.threshold,res.base)
    class(res)=c('dca.lrm','data.frame')
    res$model=factor(res$model,levels = c(model.names,'all','none'))
    res
}
thresholds.lrm <- function(fit,model.name=NULL) {
    if (is.null(model.name)) model.name=sub(' {2,}',' ',paste0(deparse(fit$terms)))
    if (class(fit)[1]=='lrm'){
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
    TP=sapply(thresholds, function(x) table(paste0(ifelse(fitted>=x,1,0),real))['11'])
    FP=sapply(thresholds, function(x) table(paste0(pred=ifelse(fitted>=x,1,0),real))['10'])
    TP[is.na(TP)]=0
    FP[is.na(FP)]=0
    names(TP)=NULL
    names(FP)=NULL
    NB=TP/length(real)-FP/length(real)*thresholds/(1-thresholds)
    TPR=TP/length(real)
    FPR=FP/length(real)
    data.frame(thresholds,TPR,FPR,NB,model=model.name)
}
base.lrm <- function(fit,thresholds) {
    real=model.data(fit)[,model.y(fit)]
    real=ifelse(real==max(real),1,0)
    # ALL IS 1
    TP=rep(table(real)['1'],length(thresholds))
    FP=rep(table(real)['0'],length(thresholds))
    TP[is.na(TP)]=0
    FP[is.na(FP)]=0
    names(TP)=NULL
    names(FP)=NULL
    NB=TP/length(real)-FP/length(real)*thresholds/(1-thresholds)
    TPR=TP/length(real)
    FPR=FP/length(real)
    df1=data.frame(thresholds,TPR,FPR,NB,model='all')
    # ALL IS 0 is 0
    TPR=FPR=NB=rep(0,length(thresholds))
    df2=data.frame(thresholds,TPR,FPR,NB,model='none')
    rbind(df1,df2)
}
