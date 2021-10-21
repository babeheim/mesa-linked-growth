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

