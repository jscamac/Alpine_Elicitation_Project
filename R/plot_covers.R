#' Plot expert based judgements of cover change
#'
#' Plot expert based judgements of cover change
#' @param data Dataframe derived from \code{summarise_plant_data()}.
#' @param type Character. Can be either species or community. Default = "species"
#' This dictates whether one is interested in community level responses or spp level responses
#' @param outfile Character. Path to save plot. Default is NULL
#' @param width width of plot. If not defined will use size of current graphic device
#' @param height height of plot. If not defined will use size of current graphic device
#' @param units Character. Can be "in", "cm", or "mm". Default is inches.
#' @importFrom dplyr ggplot2
#' @export
plot_covers <- function(data, 
                        type = "species", 
                        outfile = NULL, 
                        width = NA, 
                        height = NA,
                        units = c("in", "cm", "mm")) {
  
  colorblind_spp <- c("#009E73","#E69F00","#CC79A7","#0072B2","#D55E00")
  colorblind_comm <- c("#009E73", "#E69F00", "#CC79A7", "#F0E442","#56B4E9", "#000000", "#0072B2","#D55E00","#999999")
  
  out <- ggplot2::ggplot(data= data, ggplot2::aes(x = mean_current, y = mean_future, colour = Community)) +
    ggplot2::geom_point() + 
    ggplot2::geom_abline(intercept = 0, slope =1, linetype = 2) + 
    ggplot2::geom_errorbar(ggplot2::aes(ymin = l95ci_future, ymax=u95ci_future)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom") +
    ylab("Future Cover (%)") +
    xlab("Current Cover (%)")
  
  if(type == "species") {
    out <-  out + ggplot2::geom_errorbarh(ggplot2::aes(xmin = l95ci_current, xmax=u95ci_current)) +
      ggplot2::facet_wrap(~Community, nrow=1, scales="free_y") +
      ggplot2::scale_color_manual("",values = colorblind_spp) +
      ggplot2::scale_x_log10(limits = c(min(data$l95ci_current), max(data$u95ci_current))) +
      ggplot2::scale_y_log10(limits = c(min(data$l95ci_future), max(data$u95ci_future))) +
      geom_text_repel(aes(label=Name), colour = "grey60", size=2) 
  } else {
    out <- out + 
      ggplot2::scale_color_manual("",values = colorblind_comm)
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