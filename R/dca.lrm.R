
dca.lrm <- function(fit.list,
                model.names=NULL,
                test.harm=0,
                new.data=NULL){
    if (length(fit.list)==0) return(NULL)
    if (is.null(new.data)){
        df=lapply(1:length(fit.list),
                  function(i) thresholds.lrm(fit=fit.list[[i]],
                                             model.name = model.names[i]))
        res.threshold=do.call(rbind,df)
    }else{
        # to ensure analysised data is complete
        all.vars=unique(unlist(lapply(fit.list, function(i) all.vars(i$terms))))
        new.data=new.data[complete.cases(new.data[,all.vars]),all.vars]
        if (length(fit.list)==1){
            # if one model, we plot train and validate together
            # train
            df=lapply(1:length(fit.list),
                      function(i) thresholds.lrm(fit=fit.list[[i]],
                                                 model.name = model.names[i]))
            res.train<-do.call(rbind,df)
            # validate
            df=lapply(1:length(fit.list),
                      function(i) thresholds.lrm(fit=fit.list[[i]],
                                                 model.name = model.names[i],
                                                 new.data=new.data))
            res.validate<-do.call(rbind,df)
            res.validate$model='Validate'
            model.names=c(model.names,'Validate')
            res.threshold=rbind(res.train,res.validate)
        }else if (length(fit.list)>1){
            # if more than one model, validate will be only returned
            # validate
            df=lapply(1:length(fit.list),
                      function(i) thresholds.lrm(fit=fit.list[[i]],
                                                 model.name = model.names[i],
                                                 new.data=new.data))
            res.threshold<-do.call(rbind,df)
        }
    }

    res.threshold$NB=res.threshold$NB-test.harm
    # y is the same
    # get the most range for ref
    res.base = base.lrm(fit=fit.list[[1]],
                    thresholds=sort(unique(res.threshold$thresholds)))
    res=rbind(res.threshold,res.base)
    class(res)=c('dca.lrm','data.frame')
    res$model=factor(res$model,levels = c(model.names,'All','None'))
    res
}
thresholds.lrm <- function(fit,model.name=NULL,new.data=NULL) {
    # KEY: real and fitted
    if (is.null(model.name)) model.name=sub(' {2,}',' ',paste0(deparse(fit$terms)))
    fit = to.lrm(fit)
    if (is.null(new.data)) data=model.data(fit) else data= new.data
    real=data[,model.y(fit)]
    fitted=predict(fit,new.data=data,type = 'fitted')
    thresholds=sort(unique(fitted))
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
base.lrm <- function(fit,thresholds,new.data=NULL) {
    if (min(thresholds)>0) thresholds=c(0,thresholds)
    fit = to.lrm(fit)
    if (is.null(new.data)) data=model.data(fit) else data= new.data
    real=data[,model.y(fit)]
    real=ifelse(real==max(real),1,0)
    # All IS 1
    TP=rep(table(real)['1'],length(thresholds))
    FP=rep(table(real)['0'],length(thresholds))
    TP[is.na(TP)]=0
    FP[is.na(FP)]=0
    names(TP)=NULL
    names(FP)=NULL
    NB=TP/length(real)-FP/length(real)*thresholds/(1-thresholds)
    TPR=TP/length(real)
    FPR=FP/length(real)
    df1=data.frame(thresholds,TPR,FPR,NB,model='All')
    # All IS 0 is 0
    TPR=FPR=NB=rep(0,length(thresholds))
    df2=data.frame(thresholds,TPR,FPR,NB,model='None')
    rbind(df1,df2)
}
