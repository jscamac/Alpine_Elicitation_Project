#' Append plant species trait data to plant summary data
#'
#' Append plant species trait data to plant summary data
#' @param data Dataframe derived from summary.
#' @param trait_path Character. Path to plant species trait data
#' @importFrom dplyr
#' @export
append_plant_traits <- function(data, trait_path) {
  
  traits <- read.csv(trait_path, stringsAsFactors = FALSE) %>%
    dplyr::select(-Genus,-Species)
  
  data %>%
    dplyr::left_join(traits, by = "Species_name") %>%
    dplyr::mutate(Extent = as.numeric(ifelse(Extent == "389,816.833 km2 ", "389816", Extent)),# Fix issue with extent
                  Resprouter = dplyr::recode(Resprouter, "Y" = "Yes", "N" = "No"),
                  Resprouter = ifelse(is.na(Resprouter), "Unknown", Resprouter), # Hack to get rid of NA
                  Pollination = dplyr::recode(Pollination, "Not Wind" = "Other"),
                  Dispersal_dist_m = exp(Log10_dispersal_dist_m))
}
