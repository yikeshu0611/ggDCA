#' Plot Decision Curve
#'
#' @param data results of dca() function
#' @param color logical, whether models will be classified by color
#' @param linetype logical, whether models will be classified by line type
#' @importFrom ggplot2 ggplot aes_string geom_line ylim theme_classic xlab ylab element_blank theme
#' @importFrom ggplot2 xlim
#' @method ggplot rFP.p100
#' @return a ggplot2 picture
#' @export
ggplot.rFP.p100 <- function(data,
                           color=TRUE,
                           linetype=TRUE){

    data=as.data.frame(data)
    p <- ggplot(data,aes_string(x='thresholds',
                                y='rFP.p100',group='model'))
    if (color & linetype){
        p <- p + geom_line(aes_string(color='model',linetype='model'),
                           lwd=1.05)
    }else if (color & !linetype){
        p <- p + geom_line(aes_string(color='model'),
                           lwd=1.05)
    }else if (!color & linetype){
        p <- p + geom_line(aes_string(linetype='model'),
                           lwd=1.05)
    }else{
        stop('color and linetype can not both be FALSE')
    }

    p <- p +
        theme_classic(base_size = 15)+
        ylab('Reduction in False Positive Count(%)')+
        theme(legend.title=element_blank())
    return(p)
}

