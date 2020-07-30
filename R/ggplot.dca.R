#' Plot Decision Curve
#'
#' @param data results of dca() function
#' @importFrom ggplot2 ggplot aes_string geom_line ylim theme_classic xlab ylab element_blank theme
#' @method ggplot ggdca
#' @return a ggplot2 picture
#' @export
#'
#' @examples
#' library(rms)
#' library(ggDCA)
#'
#' # one model
#' model1 <- lrm(status~age,lung)
#' dt=dca(model1)
#' ggplot(dt)
#'
#' # 2 or more model
#' model1 <- lrm(status~age,lung)
#' model2 <- lrm(status~age+sex,lung)
#' dt=dca(model1,model2)
#' ggplot(dt)
ggplot.ggdca <- function(data){
    op = options(warn = FALSE)
    options(op)
    ylim=range(data[!data$model %in% c('none','all'),'NB'])
    ylim2=c(-max(ylim)*0.382,max(ylim))
    if (ylim2[1]>ylim[1]) ylim2[1]=ylim[1]
    data=as.data.frame(data)
    ggplot(data,aes_string(x='thresholds',y='NB'))+
        geom_line(aes_string(group='model',
                             color='model',
                             linetype='model'),lwd=1.05)+
        ylim(ylim2)+
        theme_classic(base_size = 15)+
        xlab('Risk Threshold')+
        ylab('Net Benefit')+
        theme(legend.title=element_blank())
}

