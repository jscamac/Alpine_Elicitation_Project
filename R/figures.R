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
