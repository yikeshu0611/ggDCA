
dca.cph <- function(fit.list,
                model.names=NULL,
                test.harm=0,
                times='median'){
    if (length(fit.list)==0) return(NULL)
    # if only one fit
    # means see one fit at different times
    if (length(fit.list)==1){
        if (is.null(model.names)) model.names=times
        df=lapply(1:length(times), function(i) thresholds.cph(fit=fit.list[[1]],
                                                              model.name = model.names[i],
                                                              time = times[i]))
    }else if (length(fit.list)>1){
        # see many fits at one time
        if (length(times)==1){
            if (is.null(model.names)) model.names=model.names
            df=lapply(1:length(fit.list), function(i) thresholds.cph(fit=fit.list[[i]],
                                                                     model.name = model.names[i],
                                                                     time = times))
        }else{
            stop('times can only be one when fits are many')
        }
    }
    res.threshold=do.call(rbind,df)
    res.threshold$NB=res.threshold$NB-test.harm
    # y is the same
    # get the most range for ref
    res.base = base.cph(fit=fit.list[[1]],
                        thresholds=sort(unique(res.threshold$thresholds)))
    res=rbind(res.threshold,res.base)
    class(res)=c('dca.cph','data.frame')
    res$model=factor(res$model,levels = c(model.names,'all','none'))
    res
}
thresholds.cph <- function(fit,model.name=NULL,time='median') {
    if (is.null(model.name)) model.name=sub(' {2,}',' ',paste0(deparse(fit$terms)))
    # get fitted, thresholds, real
    model.time=model.data(fit)[,model.y(fit)[1]]
    if (time=='median') time=median(model.time,na.rm = TRUE)
    surv <- rms::Survival(to.cph(fit))
    fitted=1-surv(time, fit$linear.predictors)
    thresholds=sort(unique(fitted))
    real=model.data(fit)[,model.y(fit)[2]]
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
    data.frame(thresholds,TPR,FPR,NB,model=model.name,time=time)
}
base.cph <- function(fit,thresholds) {
    if (min(thresholds)>0) thresholds=c(0,thresholds)
    real=model.data(fit)[,model.y(fit)[2]]
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
    df1=data.frame(thresholds,TPR,FPR,NB,model='all',time=NA)
    # ALL IS 0 is 0
    TPR=FPR=NB=rep(0,length(thresholds))
    df2=data.frame(thresholds,TPR,FPR,NB,model='none',time=NA)
    # rbind
    rbind(df1,df2)
}
