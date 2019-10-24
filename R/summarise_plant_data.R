#' Summarise expert data best estimates for plant data
#'
#' Summarise expert data best estimates for plant data
#'
#' @param data Dataframe derived from \code{compile_plant_data()}.
#' @param type Character. Can be either species or community. Default = "species"
#' This dictates whether one is interested in community level responses or spp level responses
#' @details This function takes percentages, logit transforms them, calculates mean
#'  and lower and upper 95 confidence intervals and then backtransforms to non-logit scale
#' @importFrom dplyr tidyr
#' @export

summarise_plant_data <- function(data, type = "species") {
  
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
    dplyr::select(Name, Community, Expert_ID, State, Q50th) %>%
    tidyr::spread(State, Q50th) %>%
    dplyr::group_by(Name, Community) %>%
    dplyr::summarise(N = n(),
                     lmn_curr = mean(qlogis(Current/100)),
                     ll95ci_curr = lmn_curr - (1.96 * sd(qlogis(Current/100))/sqrt(N)),
                     lu95ci_curr = lmn_curr + (1.96 * sd(qlogis(Current/100))/sqrt(N)),
                     lmn_fut = mean(qlogis(Future/100)),
                     ll95ci_fut = lmn_fut - (1.96 * sd(qlogis(Future/100))/sqrt(N)),
                     lu95ci_fut = lmn_fut + (1.96 * sd(qlogis(Future/100))/sqrt(N)),
                     mean_current = plogis(lmn_curr)*100,
                     l95ci_current = plogis(ll95ci_curr)*100,
                     u95ci_current = plogis(lu95ci_curr)*100,
                     mean_future = plogis(lmn_fut)*100,
                     l95ci_future = plogis(ll95ci_fut)*100,
                     u95ci_future = plogis(lu95ci_fut)*100) %>%
    dplyr::select(Name, Community, mean_current, l95ci_current, u95ci_current,
                  mean_future, l95ci_future, u95ci_future)
}