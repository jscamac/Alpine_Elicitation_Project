sources <- list.files("R/", full.names = TRUE)
for (s in sources) {
  source(s)
}
# Install packages
load_packages(packages = c("dplyr", "ggplot2", "tidyr", "drake", "cowplot","ggrepel"))

source("drake_plan/plan.R")
drake::make(plan)

