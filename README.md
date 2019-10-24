## Reproducing analysis

In order to clean and produce plots please open R in this repository and run the following:

```
source("run_drake.R")
```

This should clean, summarise and plot the data.

The workflow instructions can be found in the folder `drake_plan`.

To extract processed data simply find the relevant target in `drake_plan/plan.R` and run the following:

```
drake::readd(target_name)
```

For example, say we want to look at the species summary statistics would could obtain these by running:

```
drake::readd(plant_community_summary)
```
