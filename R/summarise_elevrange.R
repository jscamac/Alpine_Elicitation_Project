#' Summarise expert data to estimate current and future elevation range
#'
#' Summarise expert data to estimate current and future elevation range
#' using expert best estimates
#'
#' @param data Dataframe derived from \code{compile_animal_data()}.
#' @details Summarise expert data to estimate current and future elevation range
#' using expert best estimates of lower and upper elevation ranges under both
#' both current and future conditions.
#' @importFrom dplyr tidyr
#' @export

summarise_elevrange <- function(data) {
  
  
  current <- data %>%
    dplyr::filter(Q_ID %in% c("1A", "2A")) %>%
    dplyr::select(Q_ID, Expert_ID, SPP_ID, Plot_ID, Species, Species_short, Water_centric,Taxon, Mass_g, Q50th) %>%
    tidyr::spread(Q_ID, Q50th) %>%
    na.omit() %>% # Removes experts that provided no answer for particular questions
    dplyr::group_by(Species, Species_short, SPP_ID, Plot_ID,Water_centric, Taxon, Mass_g) %>%
    dplyr::summarise(N = n(),
                     mean_current = mean(`2A` - `1A`),
                     l95ci_current = mean_current - (1.96 * sd(`2A` - `1A`)/sqrt(N)),
                     u95ci_current = mean_current + (1.96 * sd(`2A` - `1A`)/sqrt(N))) %>%
    dplyr::select(-N) # Remove N because it differs between future and current
  
  future <- data %>%
    dplyr::filter(Q_ID %in% c("1B", "2B")) %>%
    dplyr::select(Q_ID, Expert_ID, SPP_ID, Plot_ID, Species, Species_short, Water_centric,Taxon, Mass_g, Q50th) %>%
    tidyr::spread(Q_ID, Q50th) %>%
    na.omit() %>% # Removes experts that provided no answer for particular questions
    dplyr::group_by(Species, Species_short, SPP_ID, Plot_ID, Water_centric, Taxon, Mass_g) %>%
    dplyr::summarise(N = n(),
                     mean_future = mean(`2B` - `1B`),
                     l95ci_future = mean_future - (1.96 * sd(`2B` - `1B`)/sqrt(N)),
                     u95ci_future = mean_future + (1.96 * sd(`2B` - `1B`)/sqrt(N))) %>%
    dplyr::select(-N) # Remove N because it differs between future and current
  
  
  dplyr::left_join(current, future, by = c("Species","Species_short","SPP_ID","Plot_ID", "Water_centric","Taxon", "Mass_g"))
}