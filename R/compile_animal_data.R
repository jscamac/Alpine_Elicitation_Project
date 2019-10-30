#' Compile animal expert data
#'
#' Compile animal expert data
#' @param expert_file Character. Path to expert answers csv file
#' @param q_file Character. Path to question meta data csv file
#' @param trait_file Character. Path to animal trait csv file
#' @importFrom dplyr
#' @export

compile_animal_data <- function(expert_file, q_file, trait_file) {
  data <- read.csv(expert_file, stringsAsFactors = FALSE)
  questions <- read.csv(q_file, stringsAsFactors = FALSE)
  traits <- read.csv(trait_file, stringsAsFactors = FALSE)
  
  data %>% 
    dplyr::left_join(questions, by = "Q_ID") %>%
    dplyr::left_join(traits, by = "SPP_ID") %>%
    dplyr::select(Expert_ID, Q_ID, Q_TYPE, SPP_ID, Species,  Water_centric, Taxon, Mass_g, Q5th, Q50th, Q95th) %>%
    dplyr::mutate(Water_centric = ifelse(Water_centric == 1, "Water centric", "Not water centric"),
                  Taxon =  factor(Taxon,
                                  levels = c("worm",
                                             "insect",
                                             "spider",
                                             "crust",
                                             "fish",
                                             "herp",
                                             "mammal"),
                                  labels = c("Worms",
                                             "Insects",
                                             "Spiders",
                                             "Crustaceans",
                                             "Fish",
                                             "Herps",
                                             "Mammals")))
}