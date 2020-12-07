#' Plot Decision Curve

#' @param data results of dca() function
#'
#' @param color logical, whether models will be classified by color
#' @param linetype logical, whether models will be classified by line type
#' @param lwd line width
#' @param mapping ignore
#' @param ... ignore
#' @param environment ignore
#' @name ggplot
#' @return a ggplot2 picture
#' @export
#'
#' @method ggplot dca.lrm
#' @export
#' @examples
#'
#' library(ggDCA)
#' library(rms)
#'
#' ######## logistic regression
#'
#' model1 <- lrm(status~ANLN,LIRI)
#' d <- dca(model1,model.names = 'ANLN')
#' ggplot(d)
#'
#' \donttest{
#'
#' model2 <- lrm(status~ANLN+CENPA,LIRI)
#' d <- dca(model2,model.names = 'ANLN+CENPA')
#' ggplot(d)
#'
#'
#' model3 <- lrm(status~ANLN+CENPA+GPR182,LIRI)
#' d <- dca(model3,model.names = 'ANLN+CENPA+GPR182')
#' ggplot(d)
#'
#' model4 <- lrm(status~ANLN+CENPA+GPR182+BCO2,LIRI)
#' d <- dca(model4,model.names = 'ANLN+CENPA+GPR182+BCO2')
#' ggplot(d)
#'
#'
#' d <- dca(model1,model2,model3,model4,
#'          model.names = c('ANLN',
#'                          'ANLN+CENPA',
#'                          'ANLN+CENPA+GPR182',
#'                          'ANLN+CENPA+GPR182+BCO2'))
#' ggplot(d,
#'        linetype = FALSE,
#'        color = c('blue','green','black','red','gray','gray'))
#'
#'
#' ##########  cox regression
#'
#' # evaluate at median time
#'
#' model1 <- coxph(Surv(time,status)~ANLN,LIRI)
#' d <- dca(model1,model.names = 'ANLN')
#' ggplot(d)
#'
#' model2 <- coxph(Surv(time,status)~ANLN+CENPA,LIRI)
#' d <- dca(model2,model.names = 'ANLN+CENPA')
#' ggplot(d)
#'
#'
#' model3 <- coxph(Surv(time,status)~ANLN+CENPA+GPR182,LIRI)
#' d <- dca(model3,model.names = 'ANLN+CENPA+GPR182')
#' ggplot(d)
#'
#' model4 <- coxph(Surv(time,status)~ANLN+CENPA+GPR182+BCO2,LIRI)
#' d <- dca(model4,model.names = 'ANLN+CENPA+GPR182+BCO2')
#' ggplot(d)
#'
#'
#' d <- dca(model1,model2,model3,model4,
#'          model.names = c('ANLN',
#'                          'ANLN+CENPA',
#'                          'ANLN+CENPA+GPR182',
#'                          'ANLN+CENPA+GPR182+BCO2'))
#' ggplot(d,
#'        linetype = FALSE,
#'        color = c('blue','green','black','red','gray','gray'))
#'
#'
#'
#' # evaluate at different times
#'
#' qt <- quantile(LIRI$time,c(0.25,0.5,0.75))
#' qt=round(qt,2)
#' model1 <- coxph(Surv(time,status)~ANLN,LIRI)
#' d <- dca(model1,
#'          model.names = 'ANLN',
#'          times = qt)
#' ggplot(d)
#'
#' model2 <- coxph(Surv(time,status)~ANLN+CENPA,LIRI)
#' d <- dca(model2,
#'          model.names = 'ANLN+CENPA',
#'          times = qt)
#' ggplot(d)
#'
#'
#' model3 <- coxph(Surv(time,status)~ANLN+CENPA+GPR182,LIRI)
#' d <- dca(model3,
#'          model.names = 'ANLN+CENPA+GPR182',
#'          times = qt)
#' ggplot(d)
#'
#' model4 <- coxph(Surv(time,status)~ANLN+CENPA+GPR182+BCO2,LIRI)
#' d <- dca(model4,
#'          model.names = 'ANLN+CENPA+GPR182+BCO2',
#'          times = qt)
#' ggplot(d)
#'
#'
#' d <- dca(model1,model2,model3,model4,
#'          model.names = c('ANLN',
#'                          'ANLN+CENPA',
#'                          'ANLN+CENPA+GPR182',
#'                          'ANLN+CENPA+GPR182+BCO2'),
#'          times = qt)
#' ggplot(d)
#' }

ggplot.dca.lrm <- function(data,
                           mapping,
                           color=TRUE,
                           linetype=TRUE,
                           lwd=1.05,
                           ...,
                           environment = parent.frame()){
    data=as.data.frame(data)
    data=data[data$thresholds!=0,]
    max=ceiling(max(data[,'NB'],na.rm = TRUE)*10)/10
    # if (max < 1) max=1
    ylim=c(-max*0.38,max)
    p <- ggplot2::ggplot(data,aes_string(x='thresholds',y='NB',group='model'))
    # both logical
    if (is.logical(color) & is.logical(linetype)){
        if (color & linetype){
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(color='model',linetype='model'),
                                   lwd=lwd)
            }else{
                p <- p + geom_line(aes_string(color='model',
                                              lwd='model',
                                              linetype='model'))+
                    scale_size_manual(values = lwd)
            }
        }else if (color & !linetype){
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(color='model'),
                                   lwd=lwd)
            }else{
                p <- p + geom_line(aes_string(color='model',
                                              lwd='model'))+
                    scale_size_manual(values = lwd)
            }
        }else if (!color & linetype){
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(linetype='model'),
                                   lwd=lwd)
            }else{
                p <- p + geom_line(aes_string(linetype='model',
                                              lwd='model'))+
                    scale_size_manual(values = lwd)
            }

        }else{
            stop('color and linetype can not both be FALSE')
        }
    }
    # logical color integer linetype
    if (is.logical(color) & is.numeric(linetype)){
        if (color & length(linetype)==1){
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(color='model'),
                                   linetype=linetype,
                                   lwd=lwd)
            }else{
                p <- p + geom_line(aes_string(color='model',lwd='model'),
                                   linetype=linetype)+
                    scale_size_manual(values = lwd)
            }

        }
        if (color & length(linetype)>1){
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(color='model',linetype='model'),
                                   lwd=lwd)+
                    scale_linetype_manual(values = linetype)
            }else{
                p <- p + geom_line(aes_string(color='model',linetype='model',
                                   lwd='model'))+
                    scale_linetype_manual(values = linetype)+
                    scale_size_manual(values = lwd)
            }

        }
        if (!color & length(linetype)==1){
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(),
                                   linetype=linetype,
                                   lwd=lwd)
            }else{
                p <- p + geom_line(aes_string(lwd='model'),
                                   linetype=linetype)+
                    scale_size_manual(values = lwd)
            }

        }
        if (!color & length(linetype)>1){
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(linetype='model'),
                                   lwd=lwd)+
                    scale_linetype_manual(values = linetype)
            }else{
                p <- p + geom_line(aes_string(linetype='model',
                                              lwd='model'))+
                    scale_linetype_manual(values = linetype)+
                    scale_size_manual(values = lwd)
            }

        }
    }
    # character color, logical linetype
    if (is.character(color) & is.logical(linetype)){
        if (length(color)==1 & linetype){
            # color='red'
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(linetype='model'),
                                   color=color,
                                   lwd=lwd)
            }else{
                p <- p + geom_line(aes_string(linetype='model',lwd='model'),
                                   color=color)+
                    scale_size_manual(values = lwd)
            }

        }
        if (length(color)>1 & linetype){
            # color=c('red','gray','black')
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(color='model',linetype='model'),
                                   lwd=lwd)+
                    scale_color_manual(values = color)
            }else{
                p <- p + geom_line(aes_string(color='model',linetype='model',
                                   lwd='model'))+
                    scale_color_manual(values = color)+
                    scale_size_manual(values = lwd)
            }

        }
        if (length(color)==1 & !linetype){
            # color='red'
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(),
                                   color=color,
                                   lwd=lwd)
            }else{
                p <- p + geom_line(aes_string(lwd='model'),
                                   color=color)+
                    scale_size_manual(values = lwd)
            }

        }
        if (length(color)>1 & !linetype){
            # color=c('red','gray','black')
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(color='model'),
                                   lwd=lwd)+
                    scale_color_manual(values = color)
            }else{
                p <- p + geom_line(aes_string(color='model',
                                              lwd='model'))+
                    scale_color_manual(values = color)+
                    scale_size_manual(values = lwd)

            }

        }
    }

    # both not logical
    if (is.character(color) & is.numeric(linetype)){
        if (length(color)==1 & length(linetype)==1){
            # color='red'
            # linetype=2
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(),
                                   linetype=linetype,
                                   color=color,
                                   lwd=lwd)
            }else{
                p <- p + geom_line(aes_string(lwd='model'),
                                   linetype=linetype,
                                   color=color)+
                    scale_size_manual(values = lwd)
            }

        }
        if (length(color)==1 & length(linetype)>1){
            # color='red'
            # linetype=c(1,2,3)
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(linetype='model'),
                                   color=color,
                                   lwd=lwd)+
                    scale_linetype_manual(values = linetype)
            }else{
                p <- p + geom_line(aes_string(linetype='model',
                                              lwd='model'),
                                   color=color)+
                    scale_linetype_manual(values = linetype)+
                    scale_size_manual(values = lwd)
            }

        }
        if (length(color)>1 & length(linetype)==1){
            # color=c('red','gray','black')
            # linetype=3
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(color='model'),
                                   linetype=linetype,
                                   lwd=lwd)+
                    scale_color_manual(values = color)
            }else{
                p <- p + geom_line(aes_string(color='model',
                                              lwd='model'),
                                   linetype=linetype)+
                    scale_color_manual(values = color)+
                    scale_size_manual(values = lwd)
            }

        }
        if (length(color)>1 & length(linetype)>1){
            # color=c('red','gray','black')
            # linetype=c(1,2,3)
            if (length(lwd)==1){
                p <- p + geom_line(aes_string(color='model',linetype='model'),
                                   lwd=lwd)+
                    scale_color_manual(values = color)+
                    scale_linetype_manual(values = linetype)
            }else{
                p <- p + geom_line(aes_string(color='model',linetype='model',
                                   lwd='model'))+
                    scale_color_manual(values = color)+
                    scale_linetype_manual(values = linetype)+
                    scale_size_manual(values = lwd)
            }

        }
    }
    p <- p +
        theme_classic(base_size = 15)+
        xlab('Risk Threshold')+
        ylab('Net Benefit')+
        theme(legend.title=element_blank())+
        ylim(ylim)
    return(p)
}

