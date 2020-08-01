#' Ranges for net benefit
#'
#' @param ... results of dca() function
#' @name range
#' @method range dca.lrm
#' @return a dataframe contains the minium and maximum of net benefit for each model
#' @export
#'
range.dca.lrm <- function(...){
    res=list(...)[[1]]
    model=levels(res$model)
    min=sapply(model, function(i) min((res$NB)[res$model==i]))
    max=sapply(model, function(i) max((res$NB)[res$model==i]))
    data.frame(min,max)
}
#' @rdname range
#' @export
range.dca.cph <- function(...){
    res=list(...)[[1]]
    model=levels(res$model)
    min=sapply(model, function(i) min((res$NB)[res$model==i]))
    max=sapply(model, function(i) max((res$NB)[res$model==i]))
    data.frame(min,max)
}
