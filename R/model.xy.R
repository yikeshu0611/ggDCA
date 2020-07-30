model.data<-function(fit){
    formu=as.formula(paste0('.~',paste0(model.x(fit),collapse = '+')))
    fit2=update(object = fit,formu,model=TRUE,x=TRUE,y=TRUE)
    if ('coxph' %in% class(fit2)){
        fit2$model$timeggg=as.numeric(fit2$model[,1])[1:nrow(fit2$model)]
        fit2$model$eventggg=as.numeric(fit2$model[,1])[-c(1:nrow(fit2$model))]
        colnames(fit2$model)[(ncol(fit2$model)-1):ncol(fit2$model)]=model.y(fit2)
        fit2$model=fit2$model[,-1]
    }
    fit2$model
}

model.y<-function(fit){
    if ('coxph' %in% class(fit)){
        all.vars(fit$terms)[c(1,2)]
    }else{
        all.vars(fit$terms)[1]
    }
}
model.x<-function(fit){
    if ('coxph' %in% class(fit)){
        all.vars(fit$terms)[-c(1,2)]
    }else{
        all.vars(fit$terms)[-1]
    }
}
