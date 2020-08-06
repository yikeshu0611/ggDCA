#' @method dca coxph
#' @export
#' @name dca
dca.coxph <- function(...,
                    model.names=do::get_names(...),
                    test.harm=0,
                    new.data=NULL,
                    times='median'){
    fit.list=list(...)
    if (length(fit.list)==0) return(NULL)
    if (length(test.harm)==1) test.harm=rep(test.harm,length(fit.list))
    all='All'
    facet='single'
    if (!is.null(new.data)){
        all.vars=unique(unlist(lapply(fit.list, function(i) all.vars(i$terms))))
        new.data=new.data[complete.cases(new.data[,all.vars]),all.vars]
    }
    # if only one fit
    # evaluate one fit at different times
    if (length(fit.list)==1){
        if (is.null(model.names)) model.names=times
        # thresholds
        dfi=lapply(1:length(times),
                   function(i) thresholds.cph(fit=fit.list[[1]],
                                              model.name = model.names[1],
                                              time = times[i],
                                              test.harm=test.harm[1],
                                              new.data=if (is.null(new.data)) model.data(fit.list[[1]]) else new.data))
        # Validate for: one time, one fit, cause we want to plot one time, one fit with validate together
        if (!is.null(new.data) & length(times)==1){
            dfi.validate=lapply(1:length(times),
                                function(i) thresholds.cph(fit=fit.list[[1]],
                                                           model.name = model.names[1],
                                                           time = times[1],
                                                           test.harm=test.harm[1],
                                                           new.data=model.data(fit.list[[1]])))
            dfi.v2=do.call(rbind,dfi.validate)
            dfi.v2$model='Validate'
            model.names=c(model.names,'Validate')
            dfi=list(rbind(dfi[[1]],dfi.v2))

        }
        if (length(times)>1){
            for(i in 1:length(times)){
                dfi.i=dfi[[i]]
                dfi.i$model=paste0(model.names,'-',times[i])
                dfi[[i]]=dfi.i
            }
            model.names=paste0(model.names,'-',times)
        }
        res.threshold=do.call(rbind,dfi)
        # y is the same
        # get the most range for ref
        dfii=lapply(1:length(times), function(i) base.cph(fit=fit.list[[1]],
                                                          time=times[i],
                                                          thresholds=res.threshold$thresholds,
                                                          new.data=if (is.null(new.data)) model.data(fit.list[[1]]) else new.data))
        if (length(times)>1){
            for(i in 1:length(times)){
                dfii.i=dfii[[i]]
                dfii.i[dfii.i$model=='All','model']=paste0('All-',times[i])
                dfii[[i]]=dfii.i
            }
            all=paste0('All-',times)
        }
        res.base =do.call(rbind,dfii)
    }else if (length(fit.list)>1){
        # evaluate many fits at one time
        if (length(times)==1){
            if (is.null(model.names)) model.names=model.names
            dfi=lapply(1:length(fit.list), function(i) thresholds.cph(fit=fit.list[[i]],
                                                                      model.name = model.names[i],
                                                                      time = times,
                                                                      test.harm=test.harm[i],
                                                                      new.data=if (is.null(new.data)) model.data(fit.list[[i]]) else new.data))
            res.threshold=do.call(rbind,dfi)
            # y is the same
            # get the most range for ref
            res.base = base.cph(fit=fit.list[[1]],
                                time=times,
                                thresholds=res.threshold$thresholds,
                                new.data=if (is.null(new.data)) model.data(fit.list[[1]]) else new.data)
        }else if(length(times)>1){
            facet='facet'
            for(j in 1:length(times)){
                if (j==1) dfi=list()
                dfi2=lapply(1:length(fit.list), function(i) thresholds.cph(fit=fit.list[[i]],
                                                                           model.name = model.names[i],
                                                                           time = times[j],
                                                                           test.harm=test.harm[i],
                                                                           new.data=if (is.null(new.data)) model.data(fit.list[[i]]) else new.data))
                dfi=c(dfi,list(do.call(rbind,dfi2)))
            }
            res.threshold=do.call(rbind,dfi)
            # y is the same
            # get the most range for ref
            base.lp=lapply(times, function(i) base.cph(fit=fit.list[[1]],
                                                       time=i,
                                                       thresholds=res.threshold$thresholds,
                                                       new.data=if (is.null(new.data)) model.data(fit.list[[1]]) else new.data))
            res.base = do.call(rbind,base.lp)
        }

    }

    res=rbind(res.threshold,res.base)
    class(res)=c('dca.cph',facet,'data.frame')
    res$model=factor(res$model,levels = c(model.names,all,'None'))
    res
}
