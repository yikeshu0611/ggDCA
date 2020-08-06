#' @method dca glm
#' @export
#' @name dca
dca.glm <- function(...,
                    model.names=do::get_names(...),
                    test.harm=0,
                    new.data=NULL){
    fit.list<-list(...)
    if (length(fit.list)==0) return(NULL)
    if (length(test.harm)==1) test.harm=rep(test.harm,length(fit.list))
    if (is.null(new.data)){
        df=lapply(1:length(fit.list),
                  function(i) thresholds.lrm(fit=fit.list[[i]],
                                             model.name = model.names[i],
                                             test.harm=test.harm[i]))
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
                                                 model.name = model.names[i],
                                                 test.harm=test.harm[i]))
            res.train<-do.call(rbind,df)
            # validate
            df=lapply(1:length(fit.list),
                      function(i) thresholds.lrm(fit=fit.list[[i]],
                                                 model.name = model.names[i],
                                                 new.data=new.data,
                                                 test.harm=test.harm[i]))
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
                                                 new.data=new.data,
                                                 test.harm=test.harm[i]))
            res.threshold<-do.call(rbind,df)
        }
    }
    # y is the same
    # get the most range for ref
    res.base = base.lrm(fit=fit.list[[1]],
                        thresholds=sort(unique(res.threshold$thresholds)))
    res=rbind(res.threshold,res.base)
    class(res)=c('dca.lrm','data.frame')
    res$model=factor(res$model,levels = c(model.names,'All','None'))
    res
}
