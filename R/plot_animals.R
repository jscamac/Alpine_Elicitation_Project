#' Plot expert based judgements of animal change
#'
#' Plot expert based judgements of animals change
#' @param data Dataframe derived from \code{summarise_animal_data()}.
#' @param type Character. Can be either species or community. Default = "species"
#' This dictates whether one is interested in community level responses or spp level responses
#' @param add_labels Logcal. Add species labels to points
#' @param facet_by Character. Name of column specifying panels
#' @param ncol Integer. Number of panel columns
#' @param outfile Character. Path to save plot. Default is NULL
#' @param width width of plot. If not defined will use size of current graphic device
#' @param height height of plot. If not defined will use size of current graphic device
#' @param units Character. Can be "in", "cm", or "mm". Default is inches.
#' @importFrom dplyr ggplot2
#' @export
plot_animals <- function(data, 
                         add_labels = TRUE,
                         facet_by = NULL,
                         ncol = 2,
                         outfile = NULL, 
                         ylabel = expression(paste("Future abundance (100",~m^2, ")")),
                         xlabel = expression(paste("Current abundance (100",~m^2, ")")),
                         width = NA, 
                         height = NA,
                         log_scale = TRUE,
                         units = c("in", "cm", "mm")) {
  
  #colorblind_pallette <- c("#D55E00", "#0072B2", "#CC79A7","#E69F00", "#000000","#009E73", "#56B4E9")
  
  out <- ggplot2::ggplot(data= data, ggplot2::aes(x = mean_current, y = mean_future, shape = Taxon)) +
    ggplot2::geom_point(aes(shape = Taxon), fill="black") + 
    ggplot2::scale_shape_manual("", values = c(0,15,1,16,23,17,25)) + # This removes the + symbol
    ggplot2::geom_abline(intercept = 0, slope =1, linetype = 2, alpha =0.5) + 
    ggplot2::geom_linerange(ggplot2::aes(ymin = l95ci_future, ymax=u95ci_future), size=0.25, alpha = 0.7) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = l95ci_current, xmax=u95ci_current), size= 0.25, alpha = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.direction = "horizontal",
                   legend.position = "bottom", panel.grid = element_blank()) +
    guides(shape = guide_legend(nrow = 1)) +
    ylab(ylabel) +
    xlab(xlabel)
  
  if(isTRUE(log_scale)) {
   out <- out + ggplot2::scale_x_log10(limits = c(min(data$l95ci_current), max(data$u95ci_current))) +
     ggplot2::scale_y_log10(limits = c(min(data$l95ci_future), max(data$u95ci_future)))
  }
  

  if(isTRUE(add_labels)) {
    out <- out + geom_text_repel(aes(label=Plot_ID), box.padding = 0.5, force=2, colour = "red", size=2, segment.alpha=1, 
                                 segment.size = 0.3,  min.segment.length = 0.001, max.overlaps =Inf)
  }
  
  if(!is.null(facet_by)) {
    out <- out + ggplot2::facet_wrap(facet_by, ncol=ncol, scales='free') 
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