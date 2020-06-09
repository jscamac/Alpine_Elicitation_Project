#' Summarise expert data best estimates to determine response directions
#'
#' Summarise expert data best estimates for plant data response directions
#'
#' @param data Dataframe derived from \code{compile_plant_data()}.
#' @param type Character. Can be either species or community. Default = "species"
#' This dictates whether one is interested in community level responses or spp level responses
#' @param trait_path Character. Path to csv file containing species trait data. Default = NULL.
#' Only relevant when type = "species".
#' @details This function takes best estimates of current and future cover and determines the number of responses indicating
#' no change, positive change or negative change
#' @importFrom dplyr tidyr
#' @export

plant_direction_frequencies <- function(data, type = "species", trait_path = NULL) {
  
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
  
  data <- data %>%
    select(Species_name, Spp_short, State, Expert_ID, Community, Q50th) %>%
    tidyr::spread(State,Q50th) %>%
    dplyr::mutate(Change = Future - Current,
                  `No change` = ifelse(Change ==0,1,0),
                  Decrease = ifelse(Change <0,1,0),
                  Increase = ifelse(Change >0,1,0)) %>%
    dplyr::group_by(Species_name, Spp_short, Community) %>%
    dplyr::summarise(N = n(),
                     `No change` = sum(`No change`),
                     Decrease = sum(Decrease),
                     Increase = sum(Increase),
                     rank = sum(Decrease)/N) %>%
    tidyr::gather(Direction, Responses, -Species_name, -Spp_short,-Community, -N, -rank) %>%
    dplyr::mutate(Direction = factor(Direction, 
                                     levels = c("Increase",
                                                "No change",
                                                "Decrease")),
                  Responses_prop = Responses/N) %>%
    ungroup()
  
  if(!is.null(trait_path) & type == "species") {
    data <- append_plant_traits(data = data,
                                trait_path = trait_path)
  }
  data
}
