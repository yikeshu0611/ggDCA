#' Plot Decision Curve

#' @param data results of dca() function
#' @param color logical, whether models will be classified by color
#' @param linetype logical, whether models will be classified by line type
#'
#' @importFrom ggplot2 ggplot aes_string geom_line ylim theme_classic xlab ylab element_blank theme
#' @importFrom ggplot2 xlim
#' @method ggplot dca.cph
#' @return a ggplot2 picture
#' @export
#'
#' @examples
#' 1+1
ggplot.dca.cph <- function(data,color=TRUE,linetype=TRUE){
    # op = options(warn = -1)
    data=as.data.frame(data)
    max=max(data[!data$model %in% c('none','all'),'NB'])
    # if (max < 1) max=1
    ylim=c(-max*0.382,max)
    p <- ggplot(data,aes_string(x='thresholds',y='NB',group='model'))
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

    p + ylim(ylim)+
        theme_classic(base_size = 15)+
        xlab('Risk Threshold')+
        ylab('Net Benefit')+
        theme(legend.title=element_blank())
    # on.exit(options(op))
}

