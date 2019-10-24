#' Plot expert based judgements of directional change
#'
#' Plot expert based judgements of directional change
#' @param data Dataframe derived from \code{direction_frequencies()}.
#' @param type Character. Can be either species or community. Default = "species"
#' This dictates whether one is interested in community level responses or spp level responses
#' @param outfile Character. Path to save plot. Default is NULL
#' @param width width of plot. If not defined will use size of current graphic device
#' @param height height of plot. If not defined will use size of current graphic device
#' @param units Character. Can be "in", "cm", or "mm". Default is inches.
#' @details This function uses the best estimate of experts to determine the frequency of directional change
#' @importFrom dplyr ggplot2
#' @export
plot_stack <- function(data, 
                       type = "species", 
                       outfile = NULL, 
                       width = NA, 
                       height = NA,
                       units = c("in", "cm", "mm")) {
  
  if(type == "species") {
    data <- data %>% 
      dplyr::mutate(lab=ifelse(Direction=='negative_change', Name, NA))
    
    p1 <- ggplot2::ggplot(data, ggplot2::aes(x=reorder(Name, negative_rank), y=Responses, label = Name)) +
      ggplot2::geom_bar(stat='identity', ggplot2::aes(fill=Direction), width= 0.5) +
      ggplot2::geom_text(ggplot2::aes(label=lab, y=0), position='identity', hjust=-0.05, vjust=-1.8, size=3, 
                         fontface='italic') +
      ggplot2::facet_wrap(~Community, ncol = 5, scale="free", shrink = FALSE)
  } else {
    data <- data %>% 
      dplyr::mutate(lab=ifelse(Direction=='negative_change', as.character(Community), NA))
    p1 <- ggplot2::ggplot(data, ggplot2::aes(x=reorder(Community, negative_rank), y=Responses, label = Community)) +
      ggplot2::geom_bar(stat='identity', ggplot2::aes(fill=Direction), width= 0.5) +
      ggplot2::geom_text(ggplot2::aes(label=lab, y=0), position='identity', size=3,  hjust=-0.05, vjust=-3)
  }
  
  out <- p1 + 
    ggplot2::scale_fill_manual(values = c(negative_change = "#d95f02", no_change = "#1b9e77", positive_change = "#7570b3"),
                               labels=c("Positive change", "No change", "Negative change")) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(size = 6, angle=45,hjust=1)) + 
    ggplot2::theme(legend.position = c(0.7,0.15)) + 
    ggplot2::scale_y_continuous(expand=c(0,0)) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Number of responses") +
    ggplot2::xlab(NULL) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(1, "cm"),
                   axis.text.y = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   axis.ticks.length.y = ggplot2::unit(0, "cm"))
  
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
