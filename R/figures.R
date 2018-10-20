# Plots species current and future covers for each expert
plot_raw_answers <- function(data, quantile ="Q50th", xlab = "Species", ylab = "Percent cover") {
  ggplot(data, aes_string(x = "Name", y = quantile, group = "State", col = "State")) +
    geom_point(position=position_dodge(width= 0.5)) +
    facet_wrap(~Community, ncol = 2, scale="free_y") +
    theme(axis.text.y = element_text(size= 7)) +
    scale_color_manual("State", values = c(Future = "#d95f02", Current = "#7570b3")) +
    xlab(xlab) +
    ylab(ylab) +
    coord_flip() + 
    theme(legend.position = c(0.7,0.15))
}

# Plots the difference (Future - Current) for each expert
plot_raw_diff <- function(data, xlab = "Species", ylab = "Change in percent cover") {
  ggplot(data, aes(x = Name, y = Difference)) +
    geom_hline(yintercept=0, col='red') +
    geom_point() +
    facet_wrap(~Community, ncol = 2, scale="free_y") +
    theme(axis.text.y = element_text(size= 7)) +
    xlab(xlab) +
    ylab(ylab) +
    coord_flip() + 
    theme(legend.position = c(0.7,0.15))
}

# Plots the coefficient of variation for each expert
plot_cv <- function(dat, stat ="mean50",xlab ="Species", ylab="Coefficient of variation") {
  
  ggplot(dat, aes_string(x = "Name", y = stat, group = "State", col = "State")) +
    geom_point(position=position_dodge(width= 0.5)) +
    facet_wrap(~Community, ncol = 2, scale="free_y") +
    theme(axis.text.y = element_text(size= 7)) +
    scale_color_manual(values = c(Future = "#d95f02", Current = "#7570b3")) +
    xlab(xlab) +
    ylab(ylab) +
    coord_flip() +
    theme(legend.position = c(0.7,0.15))
}

# Stacked plot of number of experts indicating a positive, negative or no change.
plot_stack <- function(data, xlab = "Species", ylab="Number of responses") {
  ggplot(data, aes(x=reorder(Name, -pos), y=Responses)) +
    geom_bar(stat='identity', aes(fill=Direction), width=0.5) +
    scale_fill_manual(values = c(negative_change = "#d95f02", no_change = "#1b9e77", positive_change = "#7570b3"),
                      labels=c("Positive change", "No change", "Negative change")) +
    facet_wrap(~Community, ncol = 2, scale="free_x") + 
    theme(axis.text.x=element_text(size = 6, angle=45,hjust=1)) + 
    xlab(xlab) +
    ylab(ylab) +
    theme(legend.position = c(0.7,0.15))
  
}

# Plots adapative capacity based on each expert's answer
plot_adaptive_capacity <- function(data, 
                                   xlab = "Adapt capacity -- (Future - Current)/(Future + Current)",
                                   ylab = "Species") {

ggplot(data, aes(x = adapt_cap, y = Name)) +
         geom_point() +
  facet_wrap(~Community, ncol = 2, scale="free_y") +
  theme(axis.text.y = element_text(size= 7, face = "italic")) +
  geom_vline(xintercept=0, col='red') +
  xlab(xlab) +
  ylab(ylab) +
  theme(legend.position = c(0.7,0.15))
}

# Plots expert community question answers
plot_community <- function(data, quantile ="Q50th", xlab = "Community", ylab = "Percent cover") {
  ggplot(data, aes_string(x = "Community", y = quantile, group = "State", col = "State")) +
    geom_point(position=position_dodge(width= 0.1)) +
    theme(axis.text.y = element_text(size= 7)) +
    scale_color_manual(values = c(Future = "#d95f02", Current = "#7570b3")) +
    xlab(xlab) +
    ylab(ylab) +
    coord_flip() +
    theme(legend.position = c(0.9,0.2))
}

# Plots the community difference (Future - Current) for each expert
plot_comm_diff <- function(data, xlab = "Community", ylab = "Change in percent cover") {
  ggplot(data, aes(x = Community, y = Difference)) +
    geom_hline(yintercept=0, col='red') +
    geom_point() +
    theme(axis.text.y = element_text(size= 7)) +
    xlab(xlab) +
    ylab(ylab) +
    coord_flip() + 
    theme(legend.position = c(0.7,0.15))
}

plot_scatter <- function(data, 
                         weight_type = "PW",                         
                         response, 
                         predictor, 
                         predictor_labels = NULL, 
                         response_labels = NULL,
                         ncol=1, 
                         scale ="free",
                         alpha = 0.1, 
                         xlab = NULL,
                         ylab = NULL,
                         logx = FALSE,
                         show_correlation = FALSE,
                         zero_line = FALSE,
                         ylimits = NULL) {
  
  `%>%` <- magrittr::`%>%`
  
  # Subset data by weight group used
  data <- dplyr::filter(data, Weight_type==weight_type)
  
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
    
    # Show median
    best_guess <- data[,c(response, predictor, "Name")] %>%
      mutate_(response = response) %>%
      group_by_("Name", predictor) %>%
      summarise(best_guess = median(response, na.rm=T))
    
    p1 <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data, ggplot2::aes_string(x=predictor, y=response, colour = col_by), alpha= alpha) +
      ggplot2::geom_point(data= best_guess, ggplot2::aes_string(x=predictor, y="best_guess"), col="red") +
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
    
    data <- data[,c(response, predictor, "Name")] %>%
      tidyr::gather(., key="predictor_var", value="predictor_value", -c(response, Name)) %>%
      dplyr::mutate(predictor_var = factor(predictor_var, levels=predictor, labels= predictor_labels)) %>%
      dplyr::mutate_(response = response)
    
    
    if(isTRUE(show_correlation)) {
      cor_lab <- data %>% 
        group_by(predictor_var) %>%
        summarise(cor = round(cor(response, predictor_value, use = "complete.obs",method="spearman"),3))
    }
    
    best_guess <- data %>%
      group_by(Name,predictor_var, predictor_value) %>%
      summarise(median = median(response, na.rm=T))
    
    p1 <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data, ggplot2::aes_string(x="predictor_value", y= response, colour = col_by), alpha= alpha) +
      ggplot2::geom_point(data = best_guess, ggplot2::aes(x=predictor_value, y=median), col="red") +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme_classic() +
      ggplot2::facet_wrap(~predictor_var, ncol = ncol, scales=scale, labeller =labeller(.default =ggplot2::label_parsed))
    
  }
  
  if(isTRUE(zero_line)) {
    p1 <- p1 + geom_hline(yintercept = 0, col='blue')
  }
  
  if(isTRUE(logx)) {
    p1 <- p1 + ggplot2::scale_x_log10()
  }
  
  if(isTRUE(show_correlation)) {
    p1 <- p1 + ggplot2::geom_text(data=cor_lab, aes(x = Inf, y = Inf, label=paste("r[s]==~", cor_lab$cor)),parse=TRUE, hjust=1, vjust=1.5)
  }
  
  if(!is.null(ylimits)) {
    p1 <- p1 + scale_y_continuous(limits = ylimits)
  }
  p1
}