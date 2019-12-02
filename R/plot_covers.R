#' Plot expert based judgements of cover change
#'
#' Plot expert based judgements of cover change
#' @param data Dataframe derived from \code{summarise_plant_data()}.
#' @param type Character. Can be either species or community. Default = "species"
#' This dictates whether one is interested in community level responses or spp level responses
#' @param add_labels Logical. Add species labels to points
#' @param outfile Character. Path to save plot. Default is NULL
#' @param width width of plot. If not defined will use size of current graphic device
#' @param height height of plot. If not defined will use size of current graphic device
#' @param units Character. Can be "in", "cm", or "mm". Default is inches.
#' @importFrom dplyr ggplot2
#' @export
plot_covers <- function(data, 
                        type = "species", 
                        add_labels = TRUE,
                        outfile = NULL, 
                        width = NA, 
                        height = NA,
                        units = c("in", "cm", "mm")) {
  
  colorblind_spp <- c("#009E73","#E69F00","#CC79A7","#0072B2","#D55E00")
  colorblind_comm <- c("#009E73", "#E69F00", "#CC79A7", "#F0E442","#56B4E9", "#000000", "#0072B2","#D55E00","#999999")
  
  out <- ggplot2::ggplot(data= data, ggplot2::aes(x = mean_current, y = mean_future)) +
    ggplot2::geom_point() + 
    ggplot2::geom_abline(intercept = 0, slope =1, linetype = 2, alpha = 0.5) + 
    ggplot2::geom_errorbar(ggplot2::aes(ymin = l95ci_future, ymax=u95ci_future)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = element_blank()) +
    ylab("Future Cover (%)") +
    xlab("Current Cover (%)")
  
  if(type == "species") {
    out <-  out + ggplot2::geom_errorbarh(ggplot2::aes(xmin = l95ci_current, xmax=u95ci_current)) +
      ggplot2::facet_wrap(~Community, nrow=1, scales="free_y") +
      ggplot2::scale_x_log10(limits = c(min(data$l95ci_current), max(data$u95ci_current))) +
      ggplot2::scale_y_log10(limits = c(min(data$l95ci_future), max(data$u95ci_future)))
    
    if(isTRUE(add_labels)) {
      out <- out + geom_text_repel(aes(label=Species), box.padding=1, force=15, colour = "red", size=2, segment.alpha=0.4) 
    }
    
  } else {
    out <- out + geom_text_repel(aes(label=Community), box.padding=1, force=10, colour = "red", size=2, segment.alpha=0.4) 
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