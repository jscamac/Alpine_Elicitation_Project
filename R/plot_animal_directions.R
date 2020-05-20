#' Plot expert based judgements of directional change for animals
#'
#' Plot expert based judgements of directional change for animals
#' @param data Dataframe derived from \code{animal_direction_frequencies()}.
#' @param facet_by Character. Column name in which to produce facet sub plots. Default = NULL no facetting
#' This dictates whether one is interested in community level responses or spp level responses
#' @param outfile Character. Path to save plot. Default is NULL
#' @param width width of plot. If not defined will use size of current graphic device
#' @param height height of plot. If not defined will use size of current graphic device
#' @param units Character. Can be "in", "cm", or "mm". Default is inches.
#' @details This function uses the best estimate of experts to determine the frequency of directional change
#' @importFrom dplyr ggplot2
#' @export
plot_animal_directions <- function(data,
                                  facet_by = NULL,
                                  outfile = NULL, 
                                  width = NA, 
                                  height = NA,
                                  units = c("in", "cm", "mm")) {
  
  data <- data %>% 
    dplyr::mutate(shape = sapply(Taxon, function(x) switch(x, 
                                                                 Worm = "9633",
                                                                 Insect = "9632",
                                                                 Crustacean = "9675",
                                                                 Fish = "9679", 
                                                                 Frog = "9830",
                                                                 Lizard = "9650",
                                                                 Mammal = "9660")),
                  lab = paste0(Plot_ID,". ",Species, " (", N,"; ",intToUtf8(shape, multiple=TRUE),")"),
                  lab = ifelse(Direction=="negative_change", lab, NA))
  
  p1 <- ggplot2::ggplot(data, ggplot2::aes(x=reorder(Species, negative_rank), y=Responses_prop, label = lab)) +
    ggplot2::geom_bar(stat='identity', ggplot2::aes(fill=Direction), width= 0.5) +
    scale_x_discrete(expand=c(0,1)) +
    ggplot2::geom_text(ggplot2::aes(y=0), position='identity',  size=2,  vjust = -0.7, hjust = -0.05, 
                       fontface='italic')
  if(!is.null(facet_by)) {
    if(facet_by == "Water_centric") {
      ncols <- 2 } else {
        ncols <- 3
      }
    p1 <- p1 +
      ggplot2::facet_wrap(facet_by, ncol = ncols, scale="free", shrink = TRUE)
  }
  
  out <- p1 + 
    scale_fill_grey("",start= 0.3,end = 0.8,labels=c("Increase", "No change", "Decrease")) +
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
