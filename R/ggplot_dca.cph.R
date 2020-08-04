#' Plot Decision Curve

#' @param data results of dca() function
#'
#' @param color logical, whether models will be classified by color
#' @param linetype logical, whether models will be classified by line type
#' @param lwd line width
#'
#' @importFrom ggplot2 ggplot aes_string geom_line ylim theme_classic xlab ylab element_blank theme
#' @importFrom ggplot2 facet_wrap scale_size_manual
#' @importFrom rms cph
#' @method ggplot dca.cph
#' @return a ggplot2 picture
#' @export
#'
ggplot.dca.cph <- function(data,
                           color=TRUE,
                           linetype=TRUE,
                           lwd=1.05){
    opt <- options(warn = -1)
    class=class(data)
    data=as.data.frame(data)
    max=ceiling(max(data[,'NB'],na.rm = TRUE)*10)/10
    # if (max < 1) max=1
    ylim=c(-max*0.38,max)
    p <- ggplot(data,aes_string(x='thresholds',y='NB',group='model'))
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
    if ('facet' %in% class){
        time='only for pass checking'
        p <- p+facet_wrap(~time,nrow = 1)
    }
    p <- p +
        theme_classic(base_size = 15)+
        xlab('Risk Threshold')+
        ylab('Net Benefit')+
        theme(legend.title=element_blank())+
        ylim(ylim)
    return(p)
    options(opt)
}


