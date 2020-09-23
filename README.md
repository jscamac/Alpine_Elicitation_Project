## Predicting species and community responses to global change in Australian mountain ecosystems using structured expert judgement

*By James S. Camac, Kate D.L. Umbers, John W. Morgan, Sonya R. Geange, Anca Hanea, Rachel A. Slatyer, Keith L. McDougall, Susanna E. Venn, Peter A. Vesk, Ary A. Hoffmann, Adrienne B. Nicotra*

## Summary
Conservation managers are under increasing pressure to make decisions about the allocation of finite resources to protect biodiversity under a changing climate. However, the impacts of climate and global change drivers on species are outpacing our capacity to collect the empirical data necessary to inform these decisions. This is particularly the case in the Australian Alps which has already undergone recent changes in climate and experienced more frequent large-scale bushfires. In lieu of empirical data, we used a structured expert elicitation method (the IDEA protocol) to estimate the abundance and distribution of nine vegetation groups and 89 Australian alpine and subalpine species by the year 2050. Experts predicted that most alpine vegetation communities would decline in extent by 2050; only woodlands and heathlands were predicted to increase in extent. Predicted species-level responses for alpine plants and animals were highly variable and uncertain. In general, alpine plants spanned the range of possible responses, with some expected to increase, decrease or not change in cover. By contrast, almost all animal species were predicted to decline or not change in abundance or elevation range; more species with water-centric life-cycles were expected to decline in abundance than other species. In the face of rapid change and a paucity of data, the method and outcomes outlined here provide a pragmatic and coherent basis upon which to start informing conservation policy and management, although this approach does not diminish the importance of collecting long-term ecological data. 



## What is in this repository
This repository contained the de-identified expert estimates from two expert elicitation workshops undertaken between 2017 and 2018. In addition to these data, there are also some trait level data available for most plant species. These data can be found in the `raw_data` folder.

The repository also contains the code used to summarise the data and create main text figures 1-4 as well as supplementary figures S2-S4.

## Reproducing analyses

This analysis requires R 3.6 or higher to be installed as well as `dplyr`, `drake`, `tidyr`,`ggplot2`, `ggrepel` and `cowplot` packages to be installed.

In order to maximise the reproducibility of our analyses, we've implemented the code in a make-like workflow using the R package drake.
To reporduce the analysis simply download this repository and open R in the downloaded directory. Then simply run:

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
