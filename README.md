mesa-linked-growth
============

[![License: CC BY-NC-SA 4.0](https://licensebuttons.net/l/by-nc-sa/4.0/80x15.png)](https://creativecommons.org/licenses/by-nc-sa/4.0/)

This repository contains analysis code written in R and Stan associated with "Predicting the Onset and Progression of Corornary Calcium in the Multi-Ethnic Study of Atherosclerosis". This repository is maintained by Bret Beheim and is available under Creative Commons License [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/) (see LICENSE.md for details).

## Setup

The code was developed in R v4.0.4 with the following CRAN packages:
- rlist (0.4.6.1)
- posterior (0.1.3)
- mvtnorm (1.1-2)
- dplyr (1.0.6)
- tictoc (v1.0)
- cmdstanr (v0.3.0.9000)
- rstan (v2.21.2)

Each package can be installed using the `install.packages` command in R. The {rstan} and {cmdstanr} packages use the Stan MCMC engine, which requires a C++ compiler. Installation instructions are [here](https://mc-stan.org/cmdstanr/articles/cmdstanr.html).

Additionally, this project uses the {rethinking} package, which is not on CRAN. This package can installed by using the `devtools` library, i.e. `devtools::install_github("rmcelreath/rethinking")` - installation instructions [here](http://xcelab.net/rm/software/).


## Running the analysis code

To run the primary analyses
1. In R, set the working directory to this folder (e.g. "mesa-linked-growth") containing the README.md and `run_project.R` file.
2. Type `source("run_project.R")` and hit enter.

If the required packages are installed, the code will compile the Stan models, run through each code file and return all calculations in the `figures/` folder. The script files are designed to run in sequence, but individual portions can be inspected individually. Please also note that this script must be run on a computer with administrator access because it creates intermediate subfolders within each step of execution. 

# Data Description

This analysis uses the Multi-Ethnic Study of Atherosclerosis database, which is available through the BIOLINCC request system of the [Biologic Specimen and Data Repository Information Coordination Center](https://biolincc.nhlbi.nih.gov/home/) of the National Heart, Lung and Blood Institute.

Because the analysis dataset cannot be shared publically, this repository includes a simulation engine to create a simulacrum dataset which can take the place of the real MESA archive, allowing all code to execute. Of course, the hypotheses explored in this analysis are only confirmed by running these scripts on the real dataset.

# Main Hypotheses & Predictions

This project uses a linked mixture model estimating the onset and progression of atherosclerosis, as measured by the Coronary Calcium Score. Model details are described [here](https://www.medrxiv.org/content/10.1101/2021.09.13.21263547v1). In this analysis, we ask three questions relating to the growth model:
1. are there distinct growth rates between individuals and between ethnic groups?
2. do fast-growing CAC also have earlier ages of onset?
3. does the linked-growth model make better predictions than alternatives?

To answer these questions, we test five hypotheses related to growth-model parameters, with associated empirical predictions:

- *Hypothesis 1*: there are clear differences in growth rates within group that map onto known differences in risk profiles for onset ages
  - *H1, Prediction 1*: men have higher rates of progression than women
  - *H1, Prediction 2*: at least some of the four ethnic groups are statistically distinct from the population average growth rate

- *Hypothesis 2*: all MESA patients tend to grow CAC at similar rates, but some begin growing CAC decades earlier than others
  - *H2, Prediction 1*: total variation in onset will show more variation than total variation on progression
  - *H2, Prediction 2*: individual variation in onset will show more variation than individual variation on progression

- *Hypothesis 3*: diet, physical activity, smoking, alcohol use are more important drivers of CAC onset and growth than cultural traditions or genetic variations positively associated with ethnicity in the MESA study, or sex-specific physiological or behavioral correlates
  - *H3, Prediction 1*: there is more inter-individual variation in progression rate than variation between ethnicities
  - *H3, Prediction 2*: there is more inter-individual variation in onset age than variation between ethnicities
  - *H3, Prediction 3*: there is more inter-individual variation in progression rate than variation between the sexes.
  - *H3, Prediction 4*: there is more inter-individual variation in onset age than variation between the sexes

- *Hypothesis 4*: the physiological causes of fast progression are also the causes of earlier ages of onset, and both are partly associated with sex and ethnicity
  - *H4, Prediction 1*: the individual-specific variation in onset and progression is negatively correlated

- *Hypothesis 5*: the linked-growth model captures specific physiological features of atherosclerosis progression better than more-standard linear models
  - *H5, Prediction 1*: average accuracy (measured by log-likelihood) of out-of-sample predictions is higher between sets of time-series, vs standard models
  - *H5, Prediction 2*: average accuracy (measured by log-likelihood) of out-of-sample predictions is higher both within individual patient time-series in MESA, vs standard models
