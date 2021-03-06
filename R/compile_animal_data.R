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
    dplyr::select(Expert_ID, Q_ID, Q_TYPE, SPP_ID, Plot_ID, Species,  Water_centric, Taxon, Mass_g, Q5th, Q50th, Q95th) %>%
    dplyr::filter(!SPP_ID %in% c(26,29,37)) %>%
    # Removed  V. funesta, N. montiskosciuskoi & P. blackburni as each had less than 4 expert answers for either current or future.
    # Also note that species ID in raw data misses ID 11 and ID 18 (Species were removed before elicitation).
    dplyr::mutate(
      SPP_ID = as.numeric(as.factor(Species)),# Redo ID without those species
      Water_centric = ifelse(Water_centric == 1, "Water-centric", "Not-water-centric"),
                  Taxon =  factor(Taxon,
                                  levels = c("Worm",
                                             "Insect",
                                             "Crustacean",
                                             "Fish",
                                             "Frog",
                                             "Lizard",
                                             "Mammal")),
                  Species_short = sub('^(.)\\S+', '\\1.', Species))
}