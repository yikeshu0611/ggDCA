#' Ranges for net benefit
#'
#' @param ... results of dca() function
#' @name range
#' @method range dca.lrm
#' @return a dataframe contains the minium and maximum of net benefit for each model
#' @export
#'
range.dca.lrm <- function(...){
    d=list(...)[[1]]
    d=d[d$thresholds!=0,]
    model=levels(d$model)
    min=sapply(model, function(i) min((d$NB)[d$model==i]))
    max=sapply(model, function(i) max((d$NB)[d$model==i]))
    res=list(NB=data.frame(min,max))
    min=sapply(model, function(i) min((d$thresholds)[d$model==i]))
    max=sapply(model, function(i) max((d$thresholds)[d$model==i]))
    res$thresholds=data.frame(min,max)
    res
}
#' @rdname range
#' @export
range.dca.cph <- function(...){
    d=list(...)[[1]]
    d=d[d$thresholds!=0,]
    model=levels(d$model)
    min=sapply(model, function(i) min((d$NB)[d$model==i]))
    max=sapply(model, function(i) max((d$NB)[d$model==i]))
    res=list(NB=data.frame(min,max))
    min=sapply(model, function(i) min((d$thresholds)[d$model==i]))
    max=sapply(model, function(i) max((d$thresholds)[d$model==i]))
    res$thresholds=data.frame(min,max)
    res
}
