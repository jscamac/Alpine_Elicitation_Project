#' Summarise expert data best estimates to determine response directions for animals
#'
#' Summarise expert data best estimates for plant data response directions for animals
#'
#' @param data Dataframe derived from \code{compile_animal_data()}.
#' @param Q_IDs Character vector. The pair of questions to summarise by. For example if one was interested in changes in population abundance you would
#' use c("3A", "3B"). Changes in minimum elevation would be c("1A", "1B"), and changes in upper elevation would be c("2A","2B") 
#' @details This function takes best estimates of current and future cover and determines the number of responses indicating
#' no change, positive change or negative change
#' @importFrom dplyr tidyr
#' @export

animal_spp_direction_frequencies <- function(data, Q_IDs) {
  
  data %>%
    dplyr::filter(Q_ID %in% Q_IDs) %>%
    select(Expert_ID, Species, Species_short, SPP_ID, Plot_ID, Water_centric, Taxon, Q_TYPE, Q50th) %>%
    tidyr::spread(Q_TYPE,Q50th) %>%
    na.omit %>% # Remove experts that did not provide both answers
    dplyr::mutate(Diff = Future - Current,
                  `No change` = ifelse(Diff == 0, 1,0),
                  Decrease = ifelse(Diff < 0, 1,0),
                  Increase = ifelse(Diff > 0, 1,0)) %>%
    dplyr::group_by(Species, Species_short, SPP_ID, Plot_ID, Taxon, Water_centric) %>%
    dplyr::summarise(N = n(),
                     `No change` = sum(`No change`),
                     Decrease = sum(Decrease),
                     Increase = sum(Increase),
                     rank = sum(Decrease)/N) %>%
    tidyr::gather(Direction, Responses, -Species,-Species_short, -SPP_ID, -Plot_ID, -Taxon, -Water_centric, -N, -rank) %>%
    dplyr::mutate(Direction = factor(Direction, 
                                     levels = c("Increase",
                                                "No change",
                                                "Decrease")),
                  Responses_prop = Responses/N)
}
