
rm(list = ls())

source("project_support.R")

ppl <- read.csv("people.csv")
obs <- read.csv("observations.csv")

hurdle_list <- c("hurdle", "hurdle_i", "linked_hurdle_lognormal_i")
lognormal_list <- c("lognormal", "lognormal_i", "linked_hurdle_lognormal_i")

obs$cv_set_id <- obs$within_cv_set

out <- mclapply(1:n_cv_sets, fit_training_validation, hurdle_list, lognormal_list, ppl, obs, prior_pars, mc.cores = n_cv_sets)

# collate diagnostics from each cv set
for (i in 1:length(out)) {
  if (i == 1) {
    out[[i]]$diagnostics %>% bind_rows() %>% as.data.frame() -> diagnostics
    diagnostics$validation_set <- i
    diagnostics$model <- names(out[[i]]$diagnostics)
  } else {
    out[[i]]$diagnostics %>% bind_rows() %>% as.data.frame() -> diagnostics_add
    diagnostics_add$validation_set <- i
    diagnostics_add$model <- names(out[[i]]$diagnostics)
    diagnostics <- bind_rows(diagnostics, diagnostics_add)
  }
}
diagnostics$n_ind <- n_ind
diagnostics$n_iter <- n_iter
diagnostics$chains <- 4
diagnostics$adapt_delta <- 0.8
# write.csv(diagnostics, "figures/diagnosticsCrossValidationBetween.csv", row.names = FALSE)

# assemble validation log-likelihood data frames, one for each of the two nodes, with posterior summed vll's for each model over all observations
for (i in 1:length(out)) {
  if (i == 1) {
    hurdle_vlls <- out[[i]]$hurdle_vlls
    lognormal_vlls <- out[[i]]$lognormal_vlls
  } else {
    hurdle_vll_models <- names(out[[i]]$hurdle_vlls)
    for (model in hurdle_vll_models) {
      hurdle_vlls[[model]] <- hurdle_vlls[[model]] + out[[i]]$hurdle_vlls[[model]]
    }
    lognormal_vll_models <- names(out[[i]]$lognormal_vlls)
    for (model in lognormal_vll_models) {
      lognormal_vlls[[model]] <- lognormal_vlls[[model]] + out[[i]]$lognormal_vlls[[model]]
    }
  }
}
hurdle_vlls <- as.data.frame(hurdle_vlls)
lognormal_vlls <- as.data.frame(lognormal_vlls)
# save(hurdle_vlls, lognormal_vlls, file = "./RData/between_vlls.RData")

par(mfrow = c(2, 3))
plot(hurdle_vlls[,1], type = "l", main = names(hurdle_vlls)[1])
plot(hurdle_vlls[,2], type = "l", main = names(hurdle_vlls)[2])
plot(hurdle_vlls[,3], type = "l", main = names(hurdle_vlls)[3])
plot(lognormal_vlls[,1], type = "l", main = names(lognormal_vlls)[1])
plot(lognormal_vlls[,2], type = "l", main = names(lognormal_vlls)[2])
plot(lognormal_vlls[,3], type = "l", main = names(lognormal_vlls)[3])
