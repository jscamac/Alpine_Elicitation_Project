# Combine answers with meta data
compile_data <- function(answers_path, metadata_path) {
  expert_answers <- read.csv(answers_path, stringsAsFactors = FALSE)
  metadata <- read.csv(metadata_path, stringsAsFactors = FALSE)
  
  dplyr::left_join(metadata,expert_answers, by="Q_ID") %>%
    mutate(State = factor(ifelse(State == "%cover now", "Current", 
                                 ifelse(State == "%cover in the past", "Past", "Future"))))
}

# Extracts expert species answers
extract_raw_answers <- function(compiled_data) {
  compiled_data %>%
    filter(Q_type == "Species" & Expert_type == "Expert")
}

# Extract species differences
extract_raw_differences <- function(raw_answers, stat = "Q50th") {
  
  raw_answers %>%
    select_("Name", "Community", "State", "Expert_ID", stat) %>%
    spread_("State", stat) %>%
    mutate(Difference = Future - Current)
}

# Extract expert community answers
extract_community <- function(compiled_data) {
  compiled_data %>%
    filter(Q_type == "Community" & Expert_type %in% c("Expert", NA)) %>%
    mutate(Q50th = ifelse(is.na(Q50th),Answer, Q50th),
           Community = factor(Community, 
                              levels = c("wet tussock grassland",
                                         "fen",
                                         "bog",
                                         'wetland complex',
                                         "dry heathland",
                                         "grassland/herbfield",
                                         "subalpine woodland",
                                         "snowpatch",
                                         "feldmark")))
}

# Extract community differences
extract_comm_differences <- function(community_data, stat = "Q50th") {
  
  init <- community_data %>% filter(Q_type == "Community" & 
                                      State =="Current") %>%
    select_("Community", "Current" = stat)
  
  future <- community_data %>% filter(Q_type == "Community" &
                                        State == "Future") %>%
    select_("Community", "Expert_ID", "Future" = stat)
  
  left_join(init, future, c("Community")) %>%
    mutate(Difference = Future - Current)
  
}

# Coefficient of variation
CV <- function(mean, sd){
  (sd/mean)*100
}

# Calculate CV for point estimates
Calculate_CV <- function(raw_answers) {
  
  raw_answers %>%
    group_by(Name, State, Community) %>%
    summarise(mean5 = mean(Q5th),
              sd5 = sd(Q5th),
              mean50 = mean(Q50th),
              sd50 = sd(Q50th),
              mean95 = mean(Q95th),
              sd95 = sd(Q95th)) %>%
    mutate(CV_5th = CV(mean5,sd5),
           CV_50th = CV(mean50,sd50),
           CV_95th = CV(mean95,sd95))
  }

# Calculate adaptive capacity

adaptive_capacity <- function(data, quantile = "Q50th") {
  
    data %>%
      select(Name, State, Expert_ID, Community, quantile) %>%
      tidyr::spread_("State",quantile) %>%
      mutate(adapt_cap = (Future - Current)/(Future +Current))
}

# Direction agreement

direction_agreement <- function(data) {
  data %>%
    select(Name, State, Expert_ID, Community, Q50th) %>%
    tidyr::spread(State,Q50th) %>%
    mutate(Change = Future - Current,
           no_change = ifelse(Change ==0,1,0),
           negative_change = ifelse(Change <0,1,0),
           positive_change = ifelse(Change >0,1,0)) %>%
    group_by(Name, Community) %>%
    summarise(N = n(),
              no_change = sum(no_change),
              negative_change = sum(negative_change),
              positive_change = sum(positive_change),
              pos = sum(negative_change)) %>%
    gather(Direction, Responses, -Name,-Community, -N, -pos) %>%
    mutate(Direction = factor(Direction, 
                              levels = c("positive_change",
                                         "no_change",
                                         "negative_change")))
}



