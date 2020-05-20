#' Summarise expert data best estimates for animal data
#'
#' Summarise expert data best estimates for animal data
#'
#' @param data Dataframe derived from \code{compile_animal_data()}.
#' For taxon level analysis it would be "Taxon", for water centric level analysis it would be "Water_centric". 
#' @param Q_IDs Character vector. The pair of questions to summarise by. For example if one was interested in changes in population abundance you would
#' use c("3A", "3B"). Changes in minimum elevation would be c("1A", "1B"), and changes in upper elevation would be c("2A","2B"). 
#' @param mass_convert Logical. Whether to convert abundance to total mass. Only applicable if Q_IDs = c("3A", "3B")
#' @details This function takes percentages, log transforms them, calculates mean
#'  and lower and upper 95 confidence intervals and then backtransforms to non-log scale
#' @importFrom dplyr tidyr
#' @export

summarise_animal_data <- function(data, Q_IDs, mass_convert = FALSE) {
  
  
  dat <- data %>%
    dplyr::filter(Q_ID %in% Q_IDs) %>%
    dplyr::select(Q_TYPE, Expert_ID, SPP_ID, Plot_ID, Species, Water_centric,Taxon, Mass_g, Q50th)
  
  if(identical(Q_IDs, c("3A","3B")) & isTRUE(mass_convert)) {
    dat <- dat %>%
      mutate(Q50th = Q50th * Mass_g)
  }
  
  dat %>%
    tidyr::spread(Q_TYPE, Q50th) %>%
    na.omit() %>% # Removes experts that provided no answer for particular questions
    dplyr::group_by(Species, SPP_ID,Plot_ID, Water_centric, Taxon, Mass_g) %>%
    dplyr::summarise(N = n(),
                     # Small constant added to deal with zeros in abundance and elevation data
                     lmn_curr = mean(log(Current + 0.1)),
                     ll95ci_curr = lmn_curr - (1.96 * sd(log(Current+0.1))/sqrt(N)),
                     lu95ci_curr = lmn_curr + (1.96 * sd(log(Current+0.1))/sqrt(N)),
                     lmn_fut = mean(log(Future + 0.1)),
                     ll95ci_fut = lmn_fut - (1.96 * sd(log(Future+0.1))/sqrt(N)),
                     lu95ci_fut = lmn_fut + (1.96 * sd(log(Future+0.1))/sqrt(N)),
                     mean_current = exp(lmn_curr),
                     l95ci_current = exp(ll95ci_curr),
                     u95ci_current = exp(lu95ci_curr),
                     mean_future = exp(lmn_fut),
                     l95ci_future = exp(ll95ci_fut),
                     u95ci_future = exp(lu95ci_fut)) %>%
    dplyr::select(Species, SPP_ID, Plot_ID, Water_centric, Taxon, Mass_g, N, mean_current, l95ci_current, u95ci_current,
                  mean_future, l95ci_future, u95ci_future)
}