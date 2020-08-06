
to.cph <- function(fit){
    if (class(fit)[1]=='coxph'){
        coxph2cph(fit)
    }else if (class(fit)[1]=='cph'){
        update(fit,x=TRUE,y=TRUE,model=TRUE,surv=TRUE)
    }
}
coxph2cph <-function(fit){
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
        fit=eval(parse(text=call.new))
        update(fit,surv=TRUE,x=TRUE,y=TRUE,model=TRUE)
    }else{
        stop('data must be given in formula')
    }
}
trans.base2rms <- function(call.new){
    call.new=gsub('I\\(','asis(',call.new)
    call.new=gsub('ns\\(','rcs(',call.new)
    call.new=gsub('poly\\(','pol(',call.new)
    call.new=gsub('factor\\(','catg(',call.new)
    call.new=gsub('ordered\\(','scored(',call.new)
    call.new=gsub('matrix\\(','matrx(',call.new)
    call.new=gsub('strata\\(','strat(',call.new)
    call.new
}
