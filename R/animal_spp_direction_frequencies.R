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
    select(Expert_ID, Species, Water_centric, Taxon, Q_TYPE, Q50th) %>%
    tidyr::spread(Q_TYPE,Q50th) %>%
    na.omit %>% # Remove experts that did not provide both answers
    dplyr::mutate(Diff = Future - Current,
                  no_change = ifelse(Diff == 0, 1,0),
                  negative_change = ifelse(Diff < 0, 1,0),
                  positive_change = ifelse(Diff > 0, 1,0)) %>%
    dplyr::group_by(Species, Taxon, Water_centric) %>%
    dplyr::summarise(N = n(),
                     no_change = sum(no_change),
                     negative_change = sum(negative_change),
                     positive_change = sum(positive_change),
                     negative_rank = sum(negative_change)) %>%
    tidyr::gather(Direction, Responses, -Species, -Taxon, -Water_centric, -N, -negative_rank) %>%
    dplyr::mutate(Direction = factor(Direction, 
                                     levels = c("positive_change",
                                                "no_change",
                                                "negative_change")),
                  Responses_prop = Responses/N)
}
