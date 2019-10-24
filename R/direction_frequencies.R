#' Summarise expert data best estimates to determine response directions
#'
#' Summarise expert data best estimates for plant data response directions
#'
#' @param data Dataframe derived from \code{compile_plant_data()}.
#' @param type Character. Can be either species or community. Default = "species"
#' This dictates whether one is interested in community level responses or spp level responses
#' @details This function takes percentages, logit transforms them, calculates mean
#'  and lower and upper 95 confidence intervals and then backtransforms to non-logit scale
#' @importFrom dplyr tidyr
#' @export

direction_frequencies <- function(data, type = "species") {
  
  if(!type %in% c("species", "community")) {
    stop('type must be either "species" or "community"')
  }
  
  if(type == "species") {
    data <- data %>%
      dplyr::filter(Q_type == "Species" & Expert_type == "Expert") %>%
      dplyr::mutate(Community = factor(Community,
                                       levels = c("woodland",
                                                  "heathland",
                                                  "grassland",
                                                  "wetland",
                                                  "snowpatch"),
                                       labels = c("Woodland",
                                                  "Heathland",
                                                  "Grassland",
                                                  "Wetland",
                                                  "Snowpatch")))
  } else {
    data <- data %>%
      dplyr::filter(Q_type == "Community" & Expert_type %in% c("Expert", NA)) %>%
      dplyr::mutate(Community = factor(Community, 
                                       levels = c("subalpine woodland",
                                                  "dry heathland",
                                                  "grassland/herbfield",
                                                  "wet tussock grassland",
                                                  "fen",
                                                  "bog",
                                                  "wetland complex",
                                                  "snowpatch",
                                                  "feldmark"),
                                       labels = c("Woodland",
                                                  "Heathland",
                                                  "Grassland/Herbfield",
                                                  "Wet tussock grassland",
                                                  "Fen",
                                                  "Bog",
                                                  "Wetland complex",
                                                  "Snowpatch",
                                                  "Feldmark")))
  }
  
  data %>%
    select(Name, State, Expert_ID, Community, Q50th) %>%
    tidyr::spread(State,Q50th) %>%
    dplyr::mutate(Change = Future - Current,
                  no_change = ifelse(Change ==0,1,0),
                  negative_change = ifelse(Change <0,1,0),
                  positive_change = ifelse(Change >0,1,0)) %>%
    dplyr::group_by(Name, Community) %>%
    dplyr::summarise(N = n(),
                     no_change = sum(no_change),
                     negative_change = sum(negative_change),
                     positive_change = sum(positive_change),
                     negative_rank = sum(negative_change)) %>%
    tidyr::gather(Direction, Responses, -Name,-Community, -N, -negative_rank) %>%
    dplyr::mutate(Direction = factor(Direction, 
                                     levels = c("positive_change",
                                                "no_change",
                                                "negative_change")))
}
