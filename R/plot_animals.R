#' Plot expert based judgements of animal change
#'
#' Plot expert based judgements of animals change
#' @param data Dataframe derived from \code{summarise_animal_data()}.
#' @param type Character. Can be either species or community. Default = "species"
#' This dictates whether one is interested in community level responses or spp level responses
#' @param add_labels Logcal. Add species labels to points
#' @param colour_by Character. Name of colour in which to colour points
#' @param outfile Character. Path to save plot. Default is NULL
#' @param width width of plot. If not defined will use size of current graphic device
#' @param height height of plot. If not defined will use size of current graphic device
#' @param units Character. Can be "in", "cm", or "mm". Default is inches.
#' @importFrom dplyr ggplot2
#' @export
plot_animals <- function(data, 
                         add_labels = TRUE,
                         colour_by = "Taxon",
                         outfile = NULL, 
                         ylabel = expression(paste("Future abundance (100",~m^2, ")")),
                         xlabel = expression(paste("Current abundance (100",~m^2, ")")),
                         width = NA, 
                         height = NA,
                         units = c("in", "cm", "mm")) {
  
  colorblind_pallette <- c("#D55E00", "#0072B2", "#CC79A7","#E69F00", "#000000","#009E73")
  
  out <- ggplot2::ggplot(data= data, ggplot2::aes(x = mean_current, y = mean_future, colour = !!sym(colour_by))) +
    ggplot2::geom_point() + 
    ggplot2::geom_abline(intercept = 0, slope =1, linetype = 2) + 
    ggplot2::geom_errorbar(ggplot2::aes(ymin = l95ci_future, ymax=u95ci_future)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = l95ci_current, xmax=u95ci_current)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom") +
    ylab(ylabel) +
    xlab(xlabel) +
    ggplot2::scale_x_log10(limits = c(min(data$l95ci_current), max(data$u95ci_current))) +
    ggplot2::scale_y_log10(limits = c(min(data$l95ci_future), max(data$u95ci_future))) +
    ggplot2::scale_color_manual("",values = colorblind_pallette) 

    
    if(isTRUE(add_labels)) {
      out <- out + geom_text_repel(aes(label=Species), colour = "grey60", size=2) 
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