#' @title Plot for decision curve
#' @param data resultes of dca() function
#' @param mapping ignore
#' @param color logical or colors
#' @param linetype logical or integers
#' @param lwd logical or integers
#' @param ... ignore
#' @param environment ignore
#' @rdname ggplot
#'
#' @method ggplot rFP.p100
#' @export
ggplot.rFP.p100 <- function(data,
                            mapping,
                            color=TRUE,
                            linetype=TRUE,
                            lwd=1.05,
                            ...,
                            environment = parent.frame()){
    data=as.data.frame(data)
    p <- ggplot2::ggplot(data,aes_string(x='thresholds',
                                y='rFP.p100',group='model'))
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
        ylab('Reduction in False Positive Count(%)')+
        theme(legend.title=element_blank())
    return(p)
}

