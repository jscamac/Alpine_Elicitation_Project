## Reproducing analysis
We are committed to reproducible science. As such, this repository contains all the data and code necessary to fully reproduce our results. To facilitate the reproducibility of this work, we have created a docker image and set up the entire workflow using [remake](https://github.com/richfitz/remake). Below we outline the steps required to reproduce the analyses, figures and manuscript.

## Installing remake

First install some dependencies from cran as follows:

```r
install.packages(c("R6", "yaml", "digest", "crayon", "optparse"))
```

Now we'll install some packages from [github](github.com). For this, you'll need the package [devtools](https://github.com/hadley/devtools). If you don't have devtools installed you will see an error "there is no package called 'devtools'"; if that happens install devtools with `install.packages("devtools")`.


Then install the following two packages

```r
devtools::install_github("richfitz/storr")
devtools::install_github("richfitz/remake")
```
See the info in the [remake readme](https://github.com/richfitz/remake) for further details if needed.

## Running remake locally

### Copy repository
First copy the repository to your a desired directory on you local computer. 

This can either be done using the terminal (assuming git is installed)

```
git clone https://github.com/jscamac/alpine_expert_workshop.git
```

Or can be downloaded manually by clicking [here](https://github.com/jscamac/alpine_expert_workshop/archive/master.zip).

Now open a new R session with this project set as working directory. We use a number of packages, these can be easily installed by remake:

```r
remake::install_missing_packages()
```

Then run the following to process data and generate figures:

```r
remake::make()
```

If you would like to return the processed data one can directly call the target name mentioned in the `remake.yml` file.

For example if one wished to examine the data used to produce the stacked plot one could run:

```r
dat <- remake::make("agreement")
```

They could then manually produce the stacked plot by doing:

```{r
source("R/figures.R")
plot_stack(dat)
```

**NOTE** with figures you can change the size of the figures directly in the `remake.yml` file. But you will also need to change the font size in the `R/figures.R` script
