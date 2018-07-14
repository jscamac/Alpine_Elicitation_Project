#' Create scatter plots for response vs predictors
#'
#' Create scatter plots for response vs predictors
#' @param data Dataframe
#' @param response Character vector. Column name(s) of y variable(s)
#' @param predictor Character vector. Column name(s) of x variable(s)
#' @param ncol Integer. The number of panel columns. Only relevant when there is only one response variable
#' @param scale Character. Should Scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y"). This is only relevant when multiple responses or predictors are specified
#' @param smoother Logical. Whether to add a smoother line to plot
#' @param alpha Numeric. Transparency (between 0 and 1). Low values = High transparency
#' @param xlab Character. X label
#' @param ylab Character. Y label
#' @param col_by Character. Column name.
#' @param fit_1to1line Logical. Whether to add a 1 to 1 line to plot
#' @param log_x Logical. Whether to log x axis
#' @param response_labels Character vector. A vector of column labels (in same order of response) to use in facet plot strip titles (Only relevant when multiple response specified)
#' @param predictor_labels Character vector. A vector of column labels (in same order of predictor) to use in facet plot strip titles (Only relevant when multiple predictors specified)
#' @return plot or facet plot
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
plot_scatter <- function(data, 
                         response, 
                         predictor, 
                         ncol=1, 
                         scale ="free",
                         smoother=TRUE,
                         alpha = 0.1, 
                         xlab = NULL,
                         ylab = NULL,
                         col_by = NULL,
                         fit_1to1line = FALSE,
                         log_x = FALSE,
                         predictor_labels = NULL, 
                         response_labels = NULL) {
  
  `%>%` <- magrittr::`%>%`
  
  # If single response vs single predictor
  if (length(response) ==1 & length(predictor) ==1) {
    
    if (is.null(xlab)) {
      xlab <- predictor
    }  
    if (is.null(ylab)) {
      ylab <- response
    }
    
    p1 <- ggplot2::ggplot(data, ggplot2::aes_string(x=predictor, y=response, colour = col_by)) +
      ggplot2::geom_point(alpha= alpha) +
      ggplot2::ylab(ylab) +
      ggplot2::xlab(xlab) +
      viridis::scale_colour_viridis() +
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
    
    dat <- data[,c(response,predictor, col_by)] %>%
      tidyr::gather(., key="predictor_var", value="predictor_value", -c(response, col_by)) %>%
      dplyr::mutate(predictor_var = factor(predictor_var, levels=predictor, labels= predictor_labels))
    
    
    p1 <- ggplot2::ggplot(dat, ggplot2::aes_string(x="predictor_value", y= response, colour = col_by)) +
      ggplot2::geom_point(alpha= alpha) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme_classic() +
      viridis::scale_colour_viridis() +
      ggplot2::facet_wrap(~predictor_var, ncol = ncol, scales=scale)
    
  }
  
  # If multiple responses but single predictor
  if (length(predictor) == 1 & length(response) >1) {
    
    if (is.null(xlab)) {
      xlab <- predictor
    }  
    
    if (is.null(ylab)) {
      ylab <- "Response value"
    }
    
    if (is.null(response_labels)) {
      response_labels <- response
    }
    
    dat <- data[,c(response,predictor, col_by)] %>%
      tidyr::gather(., key="response_var", value="response_value", -c(predictor, col_by)) %>%
      dplyr::mutate(response_var = factor(response_var, levels=response, labels= response_labels))
    
    p1 <- ggplot2::ggplot(dat, ggplot2::aes_string(x= predictor, y= "response_value", colour = col_by)) +
      ggplot2::geom_point(alpha= alpha) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme_classic() +
      viridis::scale_colour_viridis() +
      ggplot2::facet_wrap(~response_var, ncol = ncol, scales=scale)
  }
  
  # If multiple responses and mutliple predictors
  if (length(predictor) > 1 & length(response) > 1) {
    
    if (is.null(xlab)) {
      xlab <- "Predictor value"
    }  
    if (is.null(ylab)) {
      ylab <- "Response value"
    }
    
    if (is.null(predictor_labels)) {
      predictor_labels <- predictor
    }
    
    if (is.null(response_labels)) {
      response_labels <- response
    }
    
    dat <- data[,c(response,predictor, col_by)] %>%
      tidyr::gather(., key="predictor_var", value="predictor_value", -c(response, col_by)) %>%
      tidyr::gather(., key="response_var", value="response_value", -c(predictor_var,predictor_value, col_by)) %>%
      dplyr::mutate(predictor_var = factor(predictor_var, levels=predictor, labels= predictor_labels)) %>%
      dplyr::mutate(response_var = factor(response_var, levels=response, labels= response_labels))
    
    p1 <- ggplot2::ggplot(dat, ggplot2::aes_string(x="predictor_value", y="response_value", col = col_by)) +
      ggplot2::geom_point(alpha= alpha) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme_classic() +
      viridis::scale_colour_viridis() +
      ggplot2::facet_grid(response_var ~ predictor_var, scales=scale) +
      ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 0.5),
                     panel.spacing = ggplot2::unit(1, "lines"))
  }
  
  if(isTRUE(smoother)) {
    p1 <- p1 + ggplot2::geom_smooth(se=FALSE)
  }
  
  if(isTRUE(fit_1to1line)) {
    p1 <- p1 + geom_abline(col='red')
  }
  
  if(log_x == TRUE) {
    p1 <-  p1 + scale_x_log10()
  }
  p1
}