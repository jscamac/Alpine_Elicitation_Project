#' Compile expert plant data
#'
#' Compile expert plant data
#'
#' @param answers_path Character. Path to plant expert answers csv file.
#' @param metadata_path Character. Path to metadata on plant expert questions
#' @details This function takes percentages, logit transforms them, calculates mean
#'  and lower and upper 95 confidence intervals and then backtransforms to non-logit scale
#' @importFrom dplyr
#' @export

compile_plant_data <- function(answers_path, metadata_path) {
  expert_answers <- read.csv(answers_path, stringsAsFactors = FALSE)
  metadata <- read.csv(metadata_path, stringsAsFactors = FALSE)
  
  dplyr::left_join(metadata,expert_answers, by="Q_ID") %>%
    dplyr::mutate(State = factor(ifelse(State == "%cover now", "Current", 
                                        ifelse(State == "%cover in the past", "Current", "Future"))),
                  Q50th = ifelse(!is.na(Answer),Answer, Q50th),
                  Species = sub('^(.)\\S+', '\\1.', Name)) # Hack to fix the community data current estimates
}