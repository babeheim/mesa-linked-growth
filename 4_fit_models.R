
rm(list = ls())

source("project_support.R")

obs <- read.csv("observations.csv")
ppl <- read.csv("people.csv")

stan_data <- c(
  prior_pars, # defined in "project_support.R"
  list(
    N_ind = nrow(ppl),
    sex = as.array(ppl$sex),
    eth = as.array(ppl$eth),
    # training set
    N_obs = nrow(obs),
    pid = as.array(obs$pid),
    age_su = as.array(obs$age_su),
    y = as.array(obs$cac),
    # validation set
    N_obs_v = 0,
    pid_v = integer(),
    age_su_v = numeric(),
    y_v = numeric()
  )
)

fit <- cmdstan_models[["linked_hurdle_lognormal_i"]]$sample(parallel_chains = 4, chains = 4,
  iter_warmup = floor(n_iter/2), iter_sampling = n_iter, adapt_delta = adapt_delta,
  max_treedepth = 15, data = stan_data, step_size = 0.1,
  refresh = 100, output_dir = "samples")

fit$save_object(file = "./RData/fit.RDS")

expect_error(capture.output(diagnostics <- extract_diagnostics(fit)), NA)
diagnostics$machine_name <- machine_name
diagnostics$project_seed <- project_seed
diagnostics$n_ind <- n_ind
diagnostics$n_iter <- n_iter
diagnostics$chains <- 4
diagnostics$adapt_delta <- 0.8
write.csv(as.data.frame(diagnostics), "figures/diagnostics.csv", row.names = FALSE)
