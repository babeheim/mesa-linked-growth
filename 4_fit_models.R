
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

######

fit <- readRDS("./RData/fit.RDS")

# calc basic descriptives

ppl$in_exam1 <- ppl$pid %in% obs$pid[obs$exam == 1]
ppl$in_exam2 <- ppl$pid %in% obs$pid[obs$exam == 2]
ppl$in_exam3 <- ppl$pid %in% obs$pid[obs$exam == 3]
ppl$in_exam4 <- ppl$pid %in% obs$pid[obs$exam == 4]
ppl$in_exam5 <- ppl$pid %in% obs$pid[obs$exam == 5]
ppl$n_exams <- ppl$in_exam1 + ppl$in_exam2 + ppl$in_exam3 + ppl$in_exam4 + ppl$in_exam5

ppl$hasCacExamOne <- NA
for (i in 1:nrow(ppl)) {
  ppl$hasCacExamOne[i] <- any(obs$cac[obs$pid == ppl$pid[i] & obs$exam == 1] > 0)
}

descriptives <- list(
  nPatients = nrow(ppl),
  nObservations = nrow(obs),
  nPatientExams = length(unique(paste(obs$exam, obs$pid))),
  meanAgeExamOne = sprintf("%1.1f", mean(ppl$age_exam1)),
  stdDevAgeExamOne = sprintf("%1.1f", sd(ppl$age_exam1)),
  nOneExam = sum(ppl$n_exams == 1),
  nLongData = sum(ppl$n_exams != 1),
  nTwoExams = sum(ppl$n_exams == 2),
  nThreeExams = sum(ppl$n_exams == 3),
  nFourExams = sum(ppl$n_exams == 4),
  nFiveExams = sum(ppl$n_exams == 5),
  nWhite = sum(ppl$eth == 1),
  nAsian = sum(ppl$eth == 2),
  nBlack = sum(ppl$eth == 3),
  nHispanic = sum(ppl$eth == 4),
  percMen = paste0(round(100 * mean(ppl$sex == 1)), "\\%"),
  percHasCacFirstExam = paste0(sprintf("%1.1f", 100 * mean(ppl$hasCacExamOne)), "\\%"),
  medianCacFirstExam = sprintf("%1.1f", median(obs$cac[obs$cac > 0 & obs$exam == 1]))
)

writeLines(prep_latex_variables(descriptives), "figures/sampleDescriptives.tex")



# calculate & save key estimates

est <- calc_key_estimates(fit, ppl)

calcs <- list()
for (i in 1:length(est)) {
  if (hasName(est[[i]], "label")) {
    calc_name <- est[[i]]$label
    calcs[[paste0(calc_name, "Mean")]] <- sprintf("%1.2f", est[[i]]$mean)
    calcs[[paste0(calc_name, "SD")]] <- sprintf("%1.2f", est[[i]]$sd)
    calcs[[paste0(calc_name, "LB")]] <- sprintf("%1.2f", est[[i]]$lb)
    calcs[[paste0(calc_name, "UB")]] <- sprintf("%1.2f", est[[i]]$ub)
    calcs[[paste0(calc_name, "PSign")]] <- est[[i]]$psign
  }
}
writeLines(prep_latex_variables(calcs), "figures/keyEstimates.tex")



# prep figure/table assets

obs <- read.csv("observations.csv")
ppl <- read.csv("people.csv")

samples <- as.data.frame(as_draws_df(fit$draws()))

l_cols <- grep("^l\\[", colnames(samples))
t0_cols <- grep("^t0\\[", colnames(samples))
k_cols <- grep("^k\\[", colnames(samples))
d_cols <- grep("^d\\[", colnames(samples))

obs$age5 <- obs$age - (obs$age %% 5)

obs %>%
  group_by(age, sex) %>%
  summarize(
    pr_cac = mean(cac > 0),
    median_cac = median(cac[cac>0])
  ) %>% as.data.frame() -> obs_age_sex

obs %>%
  group_by(age5, sex) %>%
  summarize(
    pr_cac = mean(cac > 0),
    median_cac = median(cac[cac>0])
  ) %>% as.data.frame() -> obs_age5_sex

obs %>%
  group_by(age, sex, eth) %>%
  summarize(
    pr_cac = mean(cac > 0),
    median_cac = median(cac[cac>0])
  ) %>% as.data.frame() -> obs_age_sex_eth

obs %>%
  group_by(age5, sex, eth) %>%
  summarize(
    pr_cac = mean(cac > 0),
    median_cac = median(cac[cac>0])
  ) %>% as.data.frame() -> obs_age5_sex_eth

cfact <- data.frame(
  age = seq(45, 90, by = 0.1),
  pr_cac_mean = NA,
  pr_cac_lb = NA,
  pr_cac_ub = NA,
  cac_mean = NA,
  cac_lb = NA,
  cac_ub = NA
)
cfact$age_su <- (cfact$age - 50) / 10



print("make table mesaCacSample")

obs %>%
  group_by(sex) %>%
  summarize(
    N = length(unique(pid)),
    perc_cac = mean(cac > 0),
    median_cac = median(cac[cac > 0])
  ) %>% as.data.frame() -> obs_sex

obs %>%
  group_by(sex, eth) %>%
  summarize(
    N = length(unique(pid)),
    perc_cac = mean(cac > 0),
    median_cac = median(cac[cac > 0])
  ) %>% as.data.frame() -> obs_sex_eth

obs_sex$eth <- NA

mesaCacSample <- bind_rows(
  bind_rows(
    obs_sex[obs_sex$sex == 1,],
    obs_sex_eth[obs_sex_eth$sex == 1,]
  ),
  bind_rows(
    obs_sex[obs_sex$sex == 2,],
    obs_sex_eth[obs_sex_eth$sex == 2,]
  )
)

mesaCacSample$perc_cac <- sprintf("%1.1f", 100 * mesaCacSample$perc_cac)
mesaCacSample$median_cac <- sprintf("%1.1f", mesaCacSample$median_cac)

mesaCacSample <- select(mesaCacSample, -sex, -eth)

mesaCacSample$labels <- c(
  "Male", "\\hspace{2mm} White", "\\hspace{2mm} Asian", "\\hspace{2mm} Black", "\\hspace{2mm} Hispanic",
  "Female", "\\hspace{2mm} White", "\\hspace{2mm} Asian", "\\hspace{2mm} Black", "\\hspace{2mm} Hispanic")

mesaCacSample <- select(mesaCacSample, labels, everything())

colnames(mesaCacSample) <- c("", "$N$", "\\%$>$0", "Median")

mesaCacSample_tex <- mesaCacSample
mesaCacSample_tex <- rbind(colnames(mesaCacSample_tex), mesaCacSample_tex)
writeLines(texttab(mesaCacSample_tex, hlines = c(1, 6)), "./figures/mesaCacSample.tex")



print("make table sexEthnicityEstimates")

ns <- list() 
t0_calcs <- list()
d_calcs <- list()

tar <- which(ppl$sex == 1)
ns$men <- length(tar)
t0_calcs$t0_men <- summarize_samples(apply(samples[,t0_cols[tar]], 1, mean) * 10 + 50)
d_calcs$d_men <- summarize_samples(apply(samples[,d_cols[tar]], 1, mean) * 10)

tar <- which(ppl$sex == 1 & ppl$eth == 1)
ns$men_eth1 <- length(tar)
t0_calcs$t0_men_eth1 <- summarize_samples(apply(samples[,t0_cols[tar]], 1, mean) * 10 + 50)
d_calcs$d_men_eth1 <- summarize_samples(apply(samples[,d_cols[tar]], 1, mean) * 10)

tar <- which(ppl$sex == 1 & ppl$eth == 2)
ns$men_eth2 <- length(tar)
t0_calcs$t0_men_eth2 <- summarize_samples(apply(samples[,t0_cols[tar]], 1, mean) * 10 + 50)
d_calcs$d_men_eth2 <- summarize_samples(apply(samples[,d_cols[tar]], 1, mean) * 10)

tar <- which(ppl$sex == 1 & ppl$eth == 3)
ns$men_eth3 <- length(tar)
t0_calcs$t0_men_eth3 <- summarize_samples(apply(samples[,t0_cols[tar]], 1, mean) * 10 + 50)
d_calcs$d_men_eth3 <- summarize_samples(apply(samples[,d_cols[tar]], 1, mean) * 10)

tar <- which(ppl$sex == 1 & ppl$eth == 4)
ns$men_eth4 <- length(tar)
t0_calcs$t0_men_eth4 <- summarize_samples(apply(samples[,t0_cols[tar]], 1, mean) * 10 + 50)
d_calcs$d_men_eth4 <- summarize_samples(apply(samples[,d_cols[tar]], 1, mean) * 10)

tar <- which(ppl$sex == 2)
ns$women <- length(tar)
t0_calcs$t0_women <- summarize_samples(apply(samples[,t0_cols[tar]], 1, mean) * 10 + 50)
d_calcs$d_women <- summarize_samples(apply(samples[,d_cols[tar]], 1, mean) * 10)

tar <- which(ppl$sex == 2 & ppl$eth == 1)
ns$women_eth1 <- length(tar)
t0_calcs$t0_women_eth1 <- summarize_samples(apply(samples[,t0_cols[tar]], 1, mean) * 10 + 50)
d_calcs$d_women_eth1 <- summarize_samples(apply(samples[,d_cols[tar]], 1, mean) * 10)

tar <- which(ppl$sex == 2 & ppl$eth == 2)
ns$women_eth2 <- length(tar)
t0_calcs$t0_women_eth2 <- summarize_samples(apply(samples[,t0_cols[tar]], 1, mean) * 10 + 50)
d_calcs$d_women_eth2 <- summarize_samples(apply(samples[,d_cols[tar]], 1, mean) * 10)

tar <- which(ppl$sex == 2 & ppl$eth == 3)
ns$women_eth3 <- length(tar)
t0_calcs$t0_women_eth3 <- summarize_samples(apply(samples[,t0_cols[tar]], 1, mean) * 10 + 50)
d_calcs$d_women_eth3 <- summarize_samples(apply(samples[,d_cols[tar]], 1, mean) * 10)

tar <- which(ppl$sex == 2 & ppl$eth == 4)
ns$women_eth4 <- length(tar)
t0_calcs$t0_women_eth4 <- summarize_samples(apply(samples[,t0_cols[tar]], 1, mean) * 10 + 50)
d_calcs$d_women_eth4 <- summarize_samples(apply(samples[,d_cols[tar]], 1, mean) * 10)

t0_calcs %>% bind_rows() %>% as.data.frame() -> t0_calcs
d_calcs %>% bind_rows() %>% as.data.frame() -> d_calcs
ns %>% bind_rows() %>% as.numeric() -> ns

t0_years <- paste0(
  sprintf("%1.1f", t0_calcs$mean),
  " (",
  sprintf("%1.1f", t0_calcs$lb),
  "-",
  sprintf("%1.1f", t0_calcs$lb),
  ")"
)

d_years <- paste0(
  sprintf("%1.1f", d_calcs$mean),
  " (",
  sprintf("%1.1f", d_calcs$lb),
  "-",
  sprintf("%1.1f", d_calcs$lb),
  ")"
)

labels <- c(
  "Male", "\\hspace{2mm} White", "\\hspace{2mm} Asian", "\\hspace{2mm} Black", "\\hspace{2mm} Hispanic",
  "Female", "\\hspace{2mm} White", "\\hspace{2mm} Asian", "\\hspace{2mm} Black", "\\hspace{2mm} Hispanic")

sexEthnicityEstimates <- cbind(labels, ns, t0_years, d_years)
colnames(sexEthnicityEstimates) <- c("", "$N$", "$t_0$ (years)", "$d$ (years)")

# # output as tex format
sexEthnicityEstimates_tex <- sexEthnicityEstimates
sexEthnicityEstimates_tex <- rbind(colnames(sexEthnicityEstimates_tex), sexEthnicityEstimates_tex)
writeLines(texttab(sexEthnicityEstimates_tex, hlines = c(1, 6)), "./figures/sexEthnicityEstimates.tex")




print("make table varianceDecomp")

l_ind_cols <- grep("^l_ind\\[", colnames(samples))
l_sex_cols <- grep("^l_sex\\[", colnames(samples))
l_eth_cols <- grep("^l_eth\\[", colnames(samples))
k_ind_cols <- grep("^k_ind\\[", colnames(samples))
k_sex_cols <- grep("^k_sex\\[", colnames(samples))
k_eth_cols <- grep("^k_eth\\[", colnames(samples))
l_ind_var <- apply(samples[,l_ind_cols], 1, var)
l_sex_var <- apply(samples[,l_sex_cols], 1, var)
l_eth_var <- apply(samples[,l_eth_cols], 1, var)
l_total_var <- l_ind_var + l_sex_var + l_eth_var
k_ind_var <- apply(samples[,k_ind_cols], 1, var)
k_sex_var <- apply(samples[,k_sex_cols], 1, var)
k_eth_var <- apply(samples[,k_eth_cols], 1, var)
k_total_var <- k_ind_var + k_sex_var + k_eth_var
calc_var <- list(
  l_total_var = summarize_samples(l_total_var),
  l_sex_var = summarize_samples(l_sex_var),
  l_eth_var = summarize_samples(l_eth_var),
  l_ind_var = summarize_samples(l_ind_var),
  k_total_var = summarize_samples(k_total_var),
  k_sex_var = summarize_samples(k_sex_var),
  k_eth_var = summarize_samples(k_eth_var),
  k_ind_var = summarize_samples(k_ind_var)
) %>% bind_rows() %>% as.data.frame()
var_mean_hpdi <- paste0(
  sprintf("%1.1f", calc_var$mean),
  " (",
  sprintf("%1.1f", calc_var$lb),
  "-",
  sprintf("%1.1f", calc_var$ub),
  ")"
)
calc_var_ratio <- list(
  l_total_var = summarize_samples(rep(1, length(l_ind_var))),
  l_sex_var = summarize_samples(l_sex_var / l_total_var),
  l_eth_var = summarize_samples(l_eth_var / l_total_var),
  l_ind_var = summarize_samples(l_ind_var / l_total_var),
  k_total_var = summarize_samples(rep(1, length(l_ind_var))),
  k_sex_var = summarize_samples(k_sex_var / k_total_var),
  k_eth_var = summarize_samples(k_eth_var / k_total_var),
  k_ind_var = summarize_samples(k_ind_var / k_total_var)
) %>% bind_rows() %>% as.data.frame()
ratio_mean_hpdi <- paste0(
  sprintf("%1.1f", calc_var_ratio$mean),
  " (",
  sprintf("%1.1f", calc_var_ratio$lb),
  "-",
  sprintf("%1.1f", calc_var_ratio$ub),
  ")"
)
ratio_mean_hpdi[c(1, 5)] <- "-"
variable <- c("Age of Onset Total", "\\hspace{2mm} Ethnicity", "\\hspace{2mm} Sex", "\\hspace{2mm} Patient Residual",
  "Growth Rate Total", "\\hspace{2mm} Ethnicity", "\\hspace{2mm} Sex", "\\hspace{2mm} Patient Residual")
varianceDecomp <- cbind(variable, var_mean_hpdi, ratio_mean_hpdi)
colnames(varianceDecomp) <- c("", "Variance", "Ratio")
varianceDecomp_tex <- varianceDecomp
varianceDecomp_tex <- rbind(colnames(varianceDecomp_tex), varianceDecomp_tex)
writeLines(texttab(varianceDecomp_tex, hlines = c(1, 5)), "./figures/varianceDecomp.tex")







print("make figure onsetDoublingForest")

png("./figures/onsetDoublingForest.png", res = 300, units = "in", height = 5, width = 10, type = "cairo")

sex_offset <- 0.1

par(mfrow = c(1, 2))

par(mar = c(5.1, 0.2, 4.1, 0.2))

plot(NULL, xlim = c(30, 60), ylim = c(0.5, 4.5),
  xlab = "Onset Age", ylab = "", yaxt = "n",
  frame.plot = FALSE)

for (target_sex in 1:2) {
  tar <- which(ppl$sex == target_sex)
  t0_avg <- apply(samples[,t0_cols[tar]], 1, mean) * 10 + 50
  t0_avg_mean <- mean(t0_avg)
  abline(v = t0_avg_mean, col = sex_col[target_sex], lty = 2)
}

for (target_sex in 1:2) {
  for (target_eth in 1:4) {
    tar <- which(ppl$sex == target_sex & ppl$eth == target_eth)
    t0_avg <- apply(samples[,t0_cols[tar]], 1, mean) * 10 + 50
    t0_avg_mean <- mean(t0_avg)
    t0_avg_lb <- HPDI(t0_avg)[1]
    t0_avg_ub <- HPDI(t0_avg)[2]
    target_y <- target_eth + sex_offset * (-1)^(2 - target_sex)
    points(t0_avg_mean, target_y, col = sex_col[target_sex], pch = 16)
    lines(c(t0_avg_lb, t0_avg_ub), c(target_y, target_y), col = sex_col[target_sex])
    abline(h = target_eth, col = col_alpha("gray", 0.2))
    mask_text(35, target_eth, labels = eth_labels[target_eth])
  }
}

plot(NULL, xlim = c(2, 10), ylim = c(0.5, 4.5),
  xlab = "Doubling Time (Years)", ylab = "", yaxt = "n",
  frame.plot = FALSE)

for (target_sex in 1:2) {
  tar <- which(ppl$sex == target_sex)
  d_avg <- apply(samples[,d_cols[tar]], 1, mean) * 10
  d_avg_mean <- mean(d_avg)
  abline(v = d_avg_mean, col = sex_col[target_sex], lty = 2)
}

for (target_sex in 1:2) {
  for (target_eth in 1:4) {
    tar <- which(ppl$sex == target_sex & ppl$eth == target_eth)
    d_avg <- apply(samples[,d_cols[tar]], 1, mean) * 10
    d_avg_mean <- mean(d_avg)
    d_avg_lb <- HPDI(d_avg)[1]
    d_avg_ub <- HPDI(d_avg)[2]
    target_y <- target_eth + sex_offset * (-1)^(2 - target_sex)
    points(d_avg_mean, target_y, col = sex_col[target_sex], pch = 16)
    lines(c(d_avg_lb, d_avg_ub), c(target_y, target_y), col = sex_col[target_sex])
    abline(h = target_eth, col = col_alpha("gray", 0.2))
  }
}

dev.off()







print("make figure hasCacAgeSex")

png("./figures/hasCacAgeSex.png", res = 300, units = "in", height = 5, width = 5, type = "cairo")

dm <- obs_age_sex

par(mfrow = c(1, 1))

plot(NULL,
  xlim = c(45, 90), ylim = c(0, 1),
  xlab = "Patient Age", ylab = "Pr(CAC > 0)",
  main = "Probability of Nonzero CAC, MESA Full Sample",
  las = 1
)
for (target_sex in 1:2) {
  dm_tar <- which(dm$sex == target_sex)
  ppl_tar <- which(ppl$sex == target_sex)
  points(dm$age[dm_tar], dm$pr_cac[dm_tar], type = "p", pch = 16, col = sex_col[target_sex])
  tar <- which(ppl$sex == target_sex)
  l_mean <- apply(samples[,l_cols[tar]], 1, mean)
  for (i in 1:nrow(cfact)) {
    pr_cac <- logistic(samples$b * (cfact$age_su[i] - l_mean))
    cfact$pr_cac_mean[i] <- mean(pr_cac)
    cfact$pr_cac_lb[i] <- HPDI(pr_cac)[1]
    cfact$pr_cac_ub[i] <- HPDI(pr_cac)[2]
  }
  points(cfact$age, cfact$pr_cac_mean, type = "l", col = sex_col[target_sex])
  polygon(
    c(cfact$age, rev(cfact$age)), c(cfact$pr_cac_lb, rev(cfact$pr_cac_ub)),
    border = NA, col = col_alpha(sex_col[target_sex], 0.2)
  )
}

text(48.95, 0.8, labels = "men", col = sex_col[1])
text(59.4, 0.6, labels = "women", col = sex_col[2])

dev.off()



print("make figure hasCacAgeSexEthnicity")

png("./figures/hasCacAgeSexEthnicity.png", res = 300, units = "in", height = 10, width = 10, type = "cairo")

dm <- obs_age_sex_eth

par(mfrow = c(2, 2))

for (target_eth in 1:4) {
  plot(NULL,
    xlim = c(45, 90), ylim = c(0, 1),
    xlab = "Patient Age", ylab = "Pr(CAC > 0)",
    main = paste0("Probability of Nonzero CAC, MESA ", eth_labels[target_eth]),
    las = 1
  )
  for (target_sex in 1:2) {
    dm_tar <- which(dm$sex == target_sex & dm$eth == target_eth)
    ppl_tar <- which(ppl$sex == target_sex & ppl$eth == target_eth)
    points(dm$age[dm_tar], dm$pr_cac[dm_tar], type = "p", pch = 16, col = sex_col[target_sex])
    tar <- which(ppl$sex == target_sex & ppl$eth == target_eth)
    l_mean <- apply(samples[,l_cols[tar]], 1, mean)
    for (i in 1:nrow(cfact)) {
      pr_cac <- logistic(samples$b * (cfact$age_su[i] - l_mean))
      cfact$pr_cac_mean[i] <- mean(pr_cac)
      cfact$pr_cac_lb[i] <- HPDI(pr_cac)[1]
      cfact$pr_cac_ub[i] <- HPDI(pr_cac)[2]
    }
    points(cfact$age, cfact$pr_cac_mean, type = "l", col = sex_col[target_sex])
    polygon(
      c(cfact$age, rev(cfact$age)), c(cfact$pr_cac_lb, rev(cfact$pr_cac_ub)),
      border = NA, col = col_alpha(sex_col[target_sex], 0.2)
    )
  }
}

dev.off()



print("make figure medianCacAge5Sex")

png("./figures/medianCacAge5Sex.png", res = 300, units = "in", height = 5, width = 5, type = "cairo")

dm <- obs_age5_sex

par(mfrow = c(1, 1))

plot(NULL,
  xlim = c(45, 90), ylim = c(1, 1000),
  xlab = "Patient Age", ylab = "CAC (Agatston Units)",
  main = "CAC Magnitudes, MESA Full Sample",
  las = 1, log = "y"
)
for (target_sex in 1:2) {
  dm_tar <- which(dm$sex == target_sex)
  ppl_tar <- which(ppl$sex == target_sex)
  points(dm$age5[dm_tar], dm$median_cac[dm_tar], pch = 16, col = sex_col[target_sex])
  tar <- which(ppl$sex == target_sex)
  t0_mean <- apply(samples[,t0_cols[tar]], 1, mean)
  k_mean <- apply(samples[,k_cols[tar]], 1, mean)
  for (i in 1:nrow(cfact)) {
    cac <- exp(k_mean * (cfact$age_su[i] - t0_mean))
    cfact$cac_mean[i] <- mean(cac)
    cfact$cac_lb[i] <- HPDI(cac)[1]
    cfact$cac_ub[i] <- HPDI(cac)[2]
  }
  points(cfact$age, cfact$cac_mean, type = "l", col = sex_col[target_sex])
  polygon(
    c(cfact$age, rev(cfact$age)), c(cfact$cac_lb, rev(cfact$cac_ub)),
    border = NA, col = col_alpha(sex_col[target_sex], 0.2)
  )
}

dev.off()



print("make figure medianCacAgeSexEthnicity")

png("./figures/medianCacAge5SexEthnicity.png", res = 300, units = "in", height = 10, width = 10, type = "cairo")

dm <- obs_age5_sex_eth

par(mfrow = c(2, 2))

for (target_eth in 1:4) {
  plot(NULL,
    xlim = c(45, 90), ylim = c(1, 1000),
    xlab = "Patient Age", ylab = "CAC (Agatston Units)",
    main = paste0("CAC Magnitudes, MESA ", eth_labels[target_eth]),
    las = 1, log = "y"
  )
  for (target_sex in 1:2) {
    dm_tar <- which(dm$sex == target_sex & dm$eth == target_eth)
    ppl_tar <- which(ppl$sex == target_sex & ppl$eth == target_eth)
    points(dm$age5[dm_tar], dm$median_cac[dm_tar], pch = 16, col = sex_col[target_sex])
    tar <- which(ppl$sex == target_sex & ppl$eth == target_eth)
    t0_mean <- apply(samples[,t0_cols[tar]], 1, mean)
    k_mean <- apply(samples[,k_cols[tar]], 1, mean)
    for (i in 1:nrow(cfact)) {
      # exp(mu) is the *median* cac value
      cac <- exp(k_mean * (cfact$age_su[i] - t0_mean))
      cfact$cac_mean[i] <- mean(cac)
      cfact$cac_lb[i] <- HPDI(cac)[1]
      cfact$cac_ub[i] <- HPDI(cac)[2]
    }
    points(cfact$age, cfact$cac_mean, type = "l", col = sex_col[target_sex])
    polygon(
      c(cfact$age, rev(cfact$age)), c(cfact$cac_lb, rev(cfact$cac_ub)),
      border = NA, col = col_alpha(sex_col[target_sex], 0.2)
    )
  }
}

dev.off()



print("make figure onsetGrowthEthnicitySex")

png("./figures/onsetGrowthEthnicitySex.png", res = 300, units = "in", height = 5, width = 10, type = "cairo")

par(mfrow = c(1, 2))

for (target_sex in 1:2) {
  plot(NULL,
    xlim = c(0, 90), ylim = c(0, 12),
    xlab = "Patient Age", ylab = "CAC Doubling Time (Years)",
    main = paste0("CAC Onset vs. Growth, MESA ", c("Men", "Women")[target_sex]),
    las = 1
  )
  abline(v = seq(0, 80, by = 10), col = col_alpha("gray", 0.3))
  abline(h = seq(1, 12, by = 1), col = col_alpha("gray", 0.3))
  for (target_eth in 1:4) {
    tar <- which(ppl$sex == target_sex & ppl$eth == target_eth)
    t0_i_mean <- apply(samples[,t0_cols[tar]], 2, mean)
    d_i_mean <- apply(samples[,d_cols[tar]], 2, mean)
    points(t0_i_mean * 10 + 50, d_i_mean * 10, col = col_alpha(eth_col[target_eth], 0.8), pch = 16)
  }
  # which combos have >100 CAC by age 60?
  curve(10 * log(2)/log(100) * ((60-50)/10 - (x-50)/10), from = 0, to = 60, add = TRUE, lty = 2)
}

dev.off()
