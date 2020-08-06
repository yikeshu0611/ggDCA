coxph2cph <- function(fit){
    # add model
    if (class(fit) != 'coxph') stop('fit must be coxph results()')
    formu=as.formula(paste0('.~',paste0(model.x(fit),collapse = '+')))
    fit=update(object = fit,formu,model=TRUE,x=TRUE,y=TRUE)
    call=paste0(deparse(fit$call),collapse = '')
    call.new=sub('coxph','cph',call)
    call.new=trans.base2rms(call.new)
    data.name=fit$call$data
    if (!is.null(data.name)){
        fit$model=model.data(fit)
        data=paste0(deparse(data.name),'=','fit$model')
        eval(parse(text = data))
        eval(parse(text=call.new))
    }else{
        stop('data must be given in formula')
    }
}
