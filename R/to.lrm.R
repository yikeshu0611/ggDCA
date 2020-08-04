to.lrm <- function(fit){
    if (class(fit)[1]=='glm'){
        base.rms::logit2lrm(fit)
    }else{
        fit
    }
}
