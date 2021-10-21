
##### project libraries #####

library(rlist)
library(rethinking) # for HPDI
library(posterior) # for as_draws_df
library(mvtnorm)
library(dplyr)
library(parallel)
library(tictoc)
library(testthat)
library(cmdstanr)
library(tikzDevice)

##### global parameters #####

machine_name <- "mac-mini"
project_seed <- 500
n_iter <- 1000
n_ind <- 100
n_cv_sets <- 10
adapt_delta <- 0.9

set.seed(project_seed)
options(warnPartialMatchDollar=TRUE)

prior_pars <- list(
  l_pop_mu        =   0.0, # -1.0 to 4.0
  l_pop_sigma     =   0.1, # 0.1 (none) to 1 (lots)
  l_ind_sigma_mu  =   0.2, # 0.1 (none) to 0.5 (lots)
  l_sex_sigma_mu  =   0.2, # 0.1 (none) to 0.5 (lots)
  l_eth_sigma_mu  =   0.2, # 0.1 (none) to 0.5 (lots)
  lk_ind_eta      =    10, # 3 (wide) to 10 (tight)
  lk_eth_eta      =    10, # 3 (wide) to 10 (tight)
  b_mu            =   0.7, # 0.7 good variation, <0.7 too much, >0.7 too little
  b_sigma         =  0.15, # 0 to 0.2
  k_pop_mu        =   0.3, # 0.0 (slow) to 1.0 (fast)
  k_pop_sigma     =   0.2, # 0.04 (none) to 0.4 (lots)
  k_ind_sigma_mu  =   0.2, # 0.04 (none) to 0.4 (lots)
  k_sex_sigma_mu  =   0.2, # 0.04 (none) to 0.4 (lots)
  k_eth_sigma_mu  =   0.2, # 0.04 (none) to 0.4 (lots)
  s_mu            =     2  # 0.1 (none) to 5 (lots)
)

sex_col <- c("dodgerblue", "firebrick3")

# eth_col <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")
eth_col <- c("#dae318", "#2fb47c", "#2b738e", "#460e5a") # viridis
eth_labels <- c("White", "Asian", "Black", "Hispanic")

##### compile model programs #####

cmdstan_models <- list()
stan_files <- list.files("./stan", pattern = "*.stan$", full.names = TRUE)
for (i in 1:length(stan_files)) {
  model_name <- gsub(".stan", "", basename(stan_files[i]))
  cmdstan_models[[model_name]] <- cmdstan_model(stan_files[i])
}

##### project functions #####

source("R/simulation_functions.R")
source("R/estimation_functions.R")
source("R/misc_functions.R")
