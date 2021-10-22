
ppl <- read.csv("people.csv")
obs <- read.csv("observations.csv")

out <- mclapply(1:n_cv_sets, function(validation_set) {

  hurdle_list <- c("hurdle", "hurdle_i", "linked_hurdle_lognormal_i")
  lognormal_list <- c("lognormal", "lognormal_i", "linked_hurdle_lognormal_i")

  model_list <- sort(unique(c(hurdle_list, lognormal_list)))

  set_output <- list(
    hurdle_vlls = list(),
    lognormal_vlls = list(),
    diagnostics <- list()
  )

  obs$in_validation <- obs$within_cv_set == validation_set
  obs$in_training <- !obs$in_validation

  if (!any(obs$in_validation)) stop("cross-validation has no validation set!")

  stan_data <- c(
    prior_pars,
    list(
      N_ind = nrow(ppl),
      sex = as.array(ppl$sex),
      eth = as.array(ppl$eth),
      # training set
      N_obs = sum(obs$in_training),
      pid = as.array(obs$pid[obs$in_training]),
      age_su = as.array(obs$age_su[obs$in_training]),
      y = as.array(obs$cac[obs$in_training]),
      # validation set
      N_obs_v = sum(obs$in_validation),
      pid_v = as.array(obs$pid[obs$in_validation]),
      age_su_v = as.array(obs$age_su[obs$in_validation]),
      y_v = as.array(obs$cac[obs$in_validation])
    )
  )

  for (my_model in model_list) {
    fit <- cmdstan_models[[my_model]]$sample(parallel_chains = 4, chains = 4,
      iter_warmup = floor(n_iter/2), iter_sampling = n_iter, adapt_delta = adapt_delta,
      max_treedepth = 15, data = stan_data, step_size = 0.1,
      refresh = 0, show_messages = FALSE, output_dir = "samples")
    expect_error(capture.output(set_output$diagnostics[[my_model]] <- extract_diagnostics(fit)), NA)
    samples <- as.data.frame(as_draws_df(fit$draws()))
    if (my_model %in% hurdle_list) set_output$hurdle_vlls[[my_model]] <- samples$hurdle_vll
    if (my_model %in% lognormal_list) set_output$lognormal_vlls[[my_model]] <- samples$lognormal_vll
  }

  return(set_output)

  print(paste("finished within-patient cross-validation set", i))

}, mc.cores = n_cv_sets)

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

write.csv(diagnostics, "figures/diagnosticsCrossValidationWithin.csv", row.names = FALSE)

# assemble validation log-likelihood data frames, one for each of the two nodes, with posterior summed vll's for each model over all observations
for (i in 1:length(out)) {
  if (i == 1) {
    hurdle_vlls <- out[[i]]$hurdle_vlls
    lognormal_vlls <- out[[i]]$lognormal_vlls
  } else {
    for (j in 1:length(out[[i]]$hurdle_vlls)) {
      hurdle_vlls[[j]] <- hurdle_vlls[[j]] + out[[i]]$hurdle_vlls[[j]]
    }
    for (j in 1:length(out[[i]]$lognormal_vlls)) {
      lognormal_vlls[[j]] <- lognormal_vlls[[j]] + out[[i]]$lognormal_vlls[[j]]
    }
  }
}

hurdle_vlls <- as.data.frame(hurdle_vlls)
lognormal_vlls <- as.data.frame(lognormal_vlls)

save(hurdle_vlls, lognormal_vlls, file = "./RData/within_vlls.RData")

#############

load("./RData/within_vlls.RData")

hurdle_est <- list()

# add hurdle_vlls
my_model <- "linked_hurdle_lognormal_i"
hurdle_est[[my_model]] <- summarize_samples(hurdle_vlls[[my_model]])
hurdle_est[[my_model]]$label <- "crossWithinHasCACLHLN"
my_model <- "hurdle_i"
hurdle_est[[my_model]] <- summarize_samples(hurdle_vlls[[my_model]])
hurdle_est[[my_model]]$label <- "crossWithinHasCACHurdleI"
my_model <- "hurdle"
hurdle_est[[my_model]] <- summarize_samples(hurdle_vlls[[my_model]])
hurdle_est[[my_model]]$label <- "crossWithinHasCACHurdle"

lognormal_est <- list()

# add lognormal_vlls
my_model <- "linked_hurdle_lognormal_i"
lognormal_est[[my_model]] <- summarize_samples(lognormal_vlls[[my_model]])
lognormal_est[[my_model]]$label <- "crossWithinLogCACLHLN"
my_model <- "lognormal_i"
lognormal_est[[my_model]] <- summarize_samples(lognormal_vlls[[my_model]])
lognormal_est[[my_model]]$label <- "crossWithinLogCACLognormalI"
my_model <- "lognormal"
lognormal_est[[my_model]] <- summarize_samples(lognormal_vlls[[my_model]])
lognormal_est[[my_model]]$label <- "crossWithinLogCACLognormal"

calcs <- list()

for (i in 1:length(hurdle_est)) {
  if (hasName(hurdle_est[[i]], "label")) {
    calc_name <- hurdle_est[[i]]$label
    calcs[[paste0(calc_name, "Mean")]] <- sprintf("%1.2f", hurdle_est[[i]]$mean)
    calcs[[paste0(calc_name, "SD")]] <- sprintf("%1.2f", hurdle_est[[i]]$sd)
    calcs[[paste0(calc_name, "LB")]] <- sprintf("%1.2f", hurdle_est[[i]]$lb)
    calcs[[paste0(calc_name, "UB")]] <- sprintf("%1.2f", hurdle_est[[i]]$ub)
    calcs[[paste0(calc_name, "PSign")]] <- hurdle_est[[i]]$psign
  }
}

for (i in 1:length(lognormal_est)) {
  if (hasName(lognormal_est[[i]], "label")) {
    calc_name <- lognormal_est[[i]]$label
    calcs[[paste0(calc_name, "Mean")]] <- sprintf("%1.2f", lognormal_est[[i]]$mean)
    calcs[[paste0(calc_name, "SD")]] <- sprintf("%1.2f", lognormal_est[[i]]$sd)
    calcs[[paste0(calc_name, "LB")]] <- sprintf("%1.2f", lognormal_est[[i]]$lb)
    calcs[[paste0(calc_name, "UB")]] <- sprintf("%1.2f", lognormal_est[[i]]$ub)
    calcs[[paste0(calc_name, "PSign")]] <- lognormal_est[[i]]$psign
  }
}

writeLines(prep_latex_variables(calcs), "figures/crossWithinCalcs.tex")


png("./figures/crossValidationForestWithin.png", res = 300, units = "in", height = 5, width = 10)

par(mfrow = c(1, 2))

par(mar = c(5.1, 0.5, 4.1, 0.5))

vll_mean <- apply(hurdle_vlls, 2, mean)
vll_lb <- apply(hurdle_vlls, 2, HPDI)[1,]
vll_ub <- apply(hurdle_vlls, 2, HPDI)[2,]

plot(NULL, xlim = 1.5 * c(min(vll_lb), max(abs(vll_ub))), ylim = c(0.5, ncol(hurdle_vlls) + 0.5),
  xlab = "Validation Log Likelihood", ylab = "", yaxt = "n",
  frame.plot = FALSE, main = "CAC>0 Cross-Validation Scores")

abline(v = 0, lty = 2)

for (i in 1:ncol(hurdle_vlls)) {
  points(vll_mean[i], i)
  lines(c(vll_lb[i], vll_ub[i]), c(i, i))
  mask_text(vll_mean[i], i + 0.2, labels = colnames(hurdle_vlls)[i])
}

par(mar = c(5.1, 0.5, 4.1, 0.5))

vll_mean <- apply(lognormal_vlls, 2, mean)
vll_lb <- apply(lognormal_vlls, 2, HPDI)[1,]
vll_ub <- apply(lognormal_vlls, 2, HPDI)[2,]

plot(NULL, xlim = 1.5 * c(min(vll_lb), max(abs(vll_ub))), ylim = c(0.5, ncol(lognormal_vlls) + 0.5),
  xlab = "Validation Log Likelihood", ylab = "", yaxt = "n",
  frame.plot = FALSE, main = "Log(CAC) Cross-Validation Scores")

abline(v = 0, lty = 2)

for (i in 1:ncol(lognormal_vlls)) {
  points(vll_mean[i], i)
  lines(c(vll_lb[i], vll_ub[i]), c(i, i))
  mask_text(vll_mean[i], i + 0.2, labels = colnames(lognormal_vlls)[i])
}

dev.off()

