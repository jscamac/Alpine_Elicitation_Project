#' Create scatterplots
#'
#' Create scatterplots
#' @param data Dataframe.
#' @param response Character vector. Column name(s) to be response variables
#' @param predictor Character vector. Column name(s) to be predictor variables
#' @param response_label Character vector. Names to apply to predictor variables
#' @param predictor_label Character vector. Names to apply to predictor variables
#' @param ncol Integer. The number of columns to plot (only used if multiple response or predictors selected)
#' @param scale Should scales be fixed ("fixed"), free ("free"; default), or free in one dimension ("free_x", "free_y")?
#' @param alpha Integer between 0 and 1. Describes the amount of point transparency
#' @param xlab Character. X label
#' @param ylab Character. Y label
#' @param logx Logical. Whether to log x axis
#' @param show_correlation Logical. If TRUE spearman correlation is shown in each panel
#' @param zero_line Logical. If TRUE a horizontal line will be added on zero
#' @param ylimits Numeric vector. The range of ylim.
#' @param outfile Character. Path to save plot. Default is NULL
#' @param width width of plot. If not defined will use size of current graphic device
#' @param height height of plot. If not defined will use size of current graphic device
#' @param units Character. Can be "in", "cm", or "mm". Default is inches.
#' @importFrom dplyr ggplot2
#' @export

plot_scatter <- function(data, 
                         response, 
                         predictor, 
                         response_labels = NULL,
                         predictor_labels = NULL, 
                         ncol=1, 
                         scale ="free",
                         alpha = 0.8, 
                         xlab = NULL,
                         ylab = NULL,
                         logx = FALSE,
                         show_correlation = FALSE,
                         zero_line = FALSE,
                         ylimits = NULL,
                         outfile = NULL, 
                         width = NA, 
                         height = NA,
                         units = c("in", "cm", "mm")
) {
  
  # If single response vs single predictor
  if (length(response) ==1 & length(predictor) ==1) {
    
    if (is.null(xlab)) {
      xlab <- predictor
    }  
    if (is.null(ylab)) {
      ylab <- response
    }
    
    
    # Calculate spearman correlation
    
    if(isTRUE(show_correlation)) {
      cor_lab <- data.frame(cor = round(cor(data[[response]], data[[predictor]], 
                                            use = "complete.obs",method="spearman"),3))
    }
    
    p1 <- ggplot2::ggplot() +
      ggplot2::geom_point(data= data, ggplot2::aes_string(x=predictor, y=response), alpha = alpha) +
      ggplot2::ylab(ylab) +
      ggplot2::xlab(xlab) +
      ggplot2::theme_classic()
  }
  
  # If multiple predictors but single response
  
  if (length(predictor) > 1 & length(response) ==1) {
    
    if (is.null(xlab)) {
      xlab <- "Predictor value"
    }  
    
    if (is.null(ylab)) {
      ylab <- response
    }
    
    if (is.null(predictor_labels)) {
      predictor_labels <- predictor
    }
    
    data <- data[,c(response, predictor, "Species")] %>%
      tidyr::gather(., key="predictor_var", value="predictor_value", -c(response, Species)) %>%
      dplyr::mutate(predictor_var = factor(predictor_var, levels=predictor, labels= predictor_labels)) %>%
      dplyr::mutate(response = !!sym(response))
    
    
    if(isTRUE(show_correlation)) {
      cor_lab <- data %>% 
        group_by(predictor_var) %>%
        summarise(cor = round(cor(response, predictor_value, use = "complete.obs",method="spearman"),3))
    }
    
    out <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data, ggplot2::aes(x=predictor_value, y=response), alpha =alpha) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme_classic() +
      ggplot2::facet_wrap(~predictor_var, ncol = ncol, scales=scale, labeller =labeller(.default =ggplot2::label_parsed))
    
  }
  
  if(isTRUE(zero_line)) {
    out <- out + geom_hline(yintercept = 0, col='blue', linetype=2)
  }
  
  if(isTRUE(show_correlation)) {
    out <- out + ggplot2::geom_text(data=cor_lab, aes(x = Inf, y = -Inf, label=paste("r[s]==~", cor_lab$cor)),parse=TRUE, vjust = -0.5, hjust = "inward")
  }
  
  if(!is.null(ylimits)) {
    out <- out + scale_y_continuous(limits = ylimits)
  }
  
  if(isTRUE(logx)) {
    out <- out + ggplot2::scale_x_log10()
  }
  
  # outfile supplied
  if(!is.null(outfile)) {
    
    # Create directory if it does not exist
    if(!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }
    ggplot2::ggsave(filename =  outfile, width = width, height = height, units = units, plot = out)
  } else {
    out
  }
}
