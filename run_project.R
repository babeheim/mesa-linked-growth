
rm(list = ls())

source("project_support.R")
tic.clearlog()
dir_init("figures", verbose = TRUE)
dir_init("samples", verbose = TRUE)
dir_init("RData", verbose = TRUE)

tic("run mesa-linked-growth project")

# tic("run tests")
# source("0_run_tests.R")
# toc(log = TRUE)

# tic("prep real data")
# source("1_prep_data.R")
# toc(log = TRUE)

tic("simulate data")
source("1_sim_data.R")
toc(log = TRUE)

# tic("run within-patient cross-validations")
# source("2_cross_validate_within.R")
# toc(log = TRUE)

# tic("run between-patient cross-validations")
# source("3_cross_validate_between.R")
# toc(log = TRUE)

tic("fit models")
source("4_fit_models.R")
toc(log = TRUE)

toc(log = TRUE)

###########

tic.log(format = TRUE)
msg_log <- unlist(tic.log())

task <- msg_log
task <- gsub(":.*$", "", task)

time_min <- msg_log
time_min <- gsub("^.*: ", "", time_min)
time_min <- gsub(" sec elapsed", "", time_min)
time_min <- round(as.numeric(time_min)/60, 2)

report <- data.frame(
  project_seed = project_seed,
  n_iter = n_iter,
  n_ind = n_ind,
  n_cv_sets = n_cv_sets,
  machine = machine_name,
  task = task,
  time_min = time_min
)

write.csv(report, file.path("figures/timing-report.csv"), row.names = FALSE)
