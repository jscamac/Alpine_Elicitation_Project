#' Plot expert based judgements of directional change
#'
#' Plot expert based judgements of directional change
#' @param data Dataframe derived from \code{plant_direction_frequencies()}.
#' @param type Character. Can be either species or community. Default = "species"
#' This dictates whether one is interested in community level responses or spp level responses
#' @param outfile Character. Path to save plot. Default is NULL
#' @param width width of plot. If not defined will use size of current graphic device
#' @param height height of plot. If not defined will use size of current graphic device
#' @param units Character. Can be "in", "cm", or "mm". Default is inches.
#' @details This function uses the best estimate of experts to determine the frequency of directional change
#' @importFrom dplyr ggplot2
#' @export
plot_plant_directions <- function(data, 
                                  type = "species", 
                                  outfile = NULL, 
                                  width = NA, 
                                  height = NA,
                                  units = c("in", "cm", "mm")) {
  
  if(type == "species") {
    data <- data %>% 
      dplyr::mutate(lab = paste0(Species, " (", N,")"),
                    lab = ifelse(Direction=="negative_change", lab, NA),
      )
    
    p1 <- ggplot2::ggplot(data, ggplot2::aes(x=reorder(Species, negative_rank), y=Responses_prop, label = lab)) +
      ggplot2::geom_bar(stat='identity', ggplot2::aes(fill=Direction), width= 0.5) +
      ggplot2::geom_text(ggplot2::aes(y=0), position="identity", size=2, vjust = 0.5, hjust = -0.05,
                         fontface='italic') +
      ggplot2::facet_wrap(~Community, ncol = 5, scale="free", shrink = FALSE)
  } else {
    
    data <- data %>% 
      dplyr::mutate(lab = paste0(Community, " (", N,")"),
                    lab = ifelse(Direction=='negative_change', lab, NA))
    
    p1 <- ggplot2::ggplot(data, ggplot2::aes(x=reorder(Community, negative_rank), y=Responses_prop, label = lab)) +
      ggplot2::geom_bar(stat='identity', ggplot2::aes(fill=Direction), width= 0.5) +
      ggplot2::geom_text(ggplot2::aes(y=0), position= "identity", size=2, vjust = 0.5, hjust = -0.05)
  }
  
  out <- p1 + 
    ggplot2::scale_fill_manual("",values = c(negative_change = "#E69F00", no_change = "#CC79A7", positive_change = "#0072B2"),
                               labels=c("Positive change", "No change", "Negative change")) +
    ggplot2::scale_y_continuous(expand=c(0,0), breaks = c(0, .25, .5, .75, 1),
                                labels=c("0", ".25", ".5", ".75", "1")) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Proportion of responses") +
    ggplot2::xlab(NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = element_blank(),
                   panel.spacing.x = ggplot2::unit(0.25, "cm"),
                   axis.text.y = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   axis.ticks.length.y = ggplot2::unit(0, "cm"),
                   legend.position = "bottom",
                   plot.margin = unit(rep(0.5,4), "cm"))
  
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