
rm(list = ls())

source("project_support.R")

fit <- readRDS("./RData/fit.RDS")

obs <- read.csv("observations.csv")
ppl <- read.csv("people.csv")

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
  nPatients = format(nrow(ppl), big.mark = ",", trim = TRUE),
  nObservations = format(nrow(obs), big.mark = ",", trim = TRUE),
  nPatientExams = format(length(unique(paste(obs$exam, obs$pid))), big.mark = ",", trim = TRUE),
  labelWhite = eth_labels[1],
  labelAsian = eth_labels[2],
  labelBlack = eth_labels[3],
  labelHispanic = eth_labels[4],
  meanAgeExamOne = sprintf("%1.1f", mean(ppl$age_exam1)),
  stdDevAgeExamOne = sprintf("%1.1f", sd(ppl$age_exam1)),
  nOneExam = format(sum(ppl$n_exams == 1), big.mark = ",", trim = TRUE),
  nLongData = format(sum(ppl$n_exams != 1), big.mark = ",", trim = TRUE),
  nTwoExams = format(sum(ppl$n_exams == 2), big.mark = ",", trim = TRUE),
  nThreeExams = format(sum(ppl$n_exams == 3), big.mark = ",", trim = TRUE),
  nFourExams = format(sum(ppl$n_exams == 4), big.mark = ",", trim = TRUE),
  nFiveExams = format(sum(ppl$n_exams == 5), big.mark = ",", trim = TRUE),
  nWhite = format(sum(ppl$eth == 1), big.mark = ",", trim = TRUE),
  nAsian = format(sum(ppl$eth == 2), big.mark = ",", trim = TRUE),
  nBlack = format(sum(ppl$eth == 3), big.mark = ",", trim = TRUE),
  nHispanic = format(sum(ppl$eth == 4), big.mark = ",", trim = TRUE),
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

# variable for whether someone ever has cac observed during the study
ppl$cac_obs <- ppl$pid %in% obs$pid[obs$cac > 0]
obs$log_cac <- log(obs$cac)

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
    median_cac = median(cac[cac>0]),
    mean_log_cac = mean(log(cac[cac>0]))
  ) %>% as.data.frame() -> obs_age_sex

obs %>%
  group_by(age5, sex) %>%
  summarize(
    pr_cac = mean(cac > 0),
    median_cac = median(cac[cac>0]),
    mean_log_cac = mean(log(cac[cac>0]))
  ) %>% as.data.frame() -> obs_age5_sex

obs %>%
  group_by(age, sex, eth) %>%
  summarize(
    pr_cac = mean(cac > 0),
    median_cac = median(cac[cac>0]),
    mean_log_cac = mean(log(cac[cac>0]))
  ) %>% as.data.frame() -> obs_age_sex_eth

obs %>%
  group_by(age5, sex, eth) %>%
  summarize(
    pr_cac = mean(cac > 0),
    median_cac = median(cac[cac>0]),
    mean_log_cac = mean(log(cac[cac>0]))
  ) %>% as.data.frame() -> obs_age5_sex_eth

obs %>%
  filter(exam == 1) %>%
  group_by(sex) %>%
  summarize(
    N = length(unique(pid)),
    mean_age = mean(age),
    perc_cac = mean(cac > 0),
    median_cac = median(cac[cac > 0])
  ) %>% as.data.frame() -> obs_sex_exam1
obs_sex_exam1$eth <- NA

obs %>%
  filter(exam == 1) %>%
  group_by(sex, eth) %>%
  summarize(
    N = length(unique(pid)),
    mean_age = mean(age),
    perc_cac = mean(cac > 0),
    median_cac = median(cac[cac > 0])
  ) %>% as.data.frame() -> obs_sex_eth_exam1

cfact <- data.frame(
  age = seq(45, 90, by = 1),
  pr_cac_mean = NA,
  pr_cac_lb = NA,
  pr_cac_ub = NA,
  cac_median = NA,
  cac_lb = NA,
  cac_ub = NA
)
cfact$age_su <- (cfact$age - 50) / 10



print("make table mesaCacSample")

mesaCacSample <- bind_rows(
  bind_rows(
    obs_sex_exam1[obs_sex_exam1$sex == 1,],
    obs_sex_eth_exam1[obs_sex_eth_exam1$sex == 1,]
  ),
  bind_rows(
    obs_sex_exam1[obs_sex_exam1$sex == 2,],
    obs_sex_eth_exam1[obs_sex_eth_exam1$sex == 2,]
  )
)

mesaCacSample$mean_age <- sprintf("%1.1f", mesaCacSample$mean_age)
mesaCacSample$perc_cac <- sprintf("%1.1f", 100 * mesaCacSample$perc_cac)
mesaCacSample$median_cac <- sprintf("%1.1f", mesaCacSample$median_cac)

mesaCacSample <- select(mesaCacSample, -sex, -eth)

mesaCacSample$labels <- c(
  "Male", paste("\\hspace{2mm}", eth_labels),
  "Female", paste("\\hspace{2mm}", eth_labels))

mesaCacSample <- select(mesaCacSample, labels, everything())

colnames(mesaCacSample) <- c("", "$N$", "Age", "\\%$>$0", "Median")

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
  sprintf("%1.1f", t0_calcs$ub),
  ")"
)

d_years <- paste0(
  sprintf("%1.1f", d_calcs$mean),
  " (",
  sprintf("%1.1f", d_calcs$lb),
  "-",
  sprintf("%1.1f", d_calcs$ub),
  ")"
)

labels <- c(
  "Male", paste("\\hspace{2mm}", eth_labels),
  "Female", paste("\\hspace{2mm}", eth_labels))

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


print("make figure samplingChains")

png("./figures/samplingChains1.png", res = 300, units = "in", height = 10, width = 15, type = "cairo")

par(mfrow = c(2, 3))

chain_plotter("lp__", samples)
chain_plotter("l_pop", samples)
chain_plotter("k_pop", samples)
chain_plotter("s", samples)
chain_plotter("b", samples)

dev.off()



png("./figures/samplingChains2.png", res = 300, units = "in", height = 10, width = 15, type = "cairo")

par(mfrow = c(2, 3))

chain_plotter("l_ind_sigma", samples)
chain_plotter("l_eth_sigma", samples)
chain_plotter("l_sex_sigma", samples)
chain_plotter("k_ind_sigma", samples)
chain_plotter("k_eth_sigma", samples)
chain_plotter("k_sex_sigma", samples)

dev.off()



png("./figures/samplingChains3.png", res = 300, units = "in", height = 10, width = 15, type = "cairo")

par(mfrow = c(2, 3))

target_ppl <- sample(1:nrow(ppl), 6)

for (i in 1:length(target_ppl)) {
  chain_plotter(paste0("l[", target_ppl[i], "]"), samples)
}

dev.off()


png("./figures/samplingChains4.png", res = 300, units = "in", height = 10, width = 15, type = "cairo")

par(mfrow = c(2, 3))

target_ppl <- sample(1:nrow(ppl), 6)

for (i in 1:length(target_ppl)) {
  chain_plotter(paste0("k[", target_ppl[i], "]"), samples)
}

dev.off()


png("./figures/samplingChains5.png", res = 300, units = "in", height = 10, width = 15, type = "cairo")

par(mfrow = c(2, 3))

target_ppl <- sample(1:nrow(ppl), 6)

for (i in 1:length(target_ppl)) {
  chain_plotter(paste0("t0[", target_ppl[i], "]"), samples)
}

dev.off()



png("./figures/samplingChains6.png", res = 300, units = "in", height = 10, width = 15, type = "cairo")

par(mfrow = c(2, 3))

target_ppl <- sample(1:nrow(ppl), 6)

for (i in 1:length(target_ppl)) {
  chain_plotter(paste0("d[", target_ppl[i], "]"), samples)
}

dev.off()



print("make figure onsetGrowthMarginalForest")

png("./figures/onsetGrowthMarginalForest.png", res = 300, units = "in", height = 5, width = 10, type = "cairo")

sex_offset <- 0.1

par(mfrow = c(1, 2))

par(mar = c(5.1, 0.2, 4.1, 0.2))

plot(NULL, xlim = c(30, 90), ylim = c(0.5, 4.5),
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

plot(NULL, xlim = c(2, 5), ylim = c(0.5, 4.5),
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



print("make figure hasCacOnsetGrowthSexEth")

# png("./figures/hasCacOnsetGrowthSexEth.png", res = 300, units = "in", height = 5, width = 10, type = "cairo")
tikz("./figures/hasCacOnsetGrowthSexEth.tex", height = 3.7, width = 7)

par(mfrow = c(1, 2))

dm <- obs_age5_sex

plot(NULL,
  xlim = c(45, 90), ylim = c(0, 1),
  xlab = "Patient Age", ylab = "Pr(CAC $>$ 0)",
  las = 1
)
abline(v = seq(40, 95, by = 5), col = gray(0.8))
abline(h = seq(0, 1, by = 0.1), col = gray(0.8))
for (target_sex in 1:2) {
  dm_tar <- which(dm$sex == target_sex)
  ppl_tar <- which(ppl$sex == target_sex)
  points(dm$age5[dm_tar], dm$pr_cac[dm_tar], type = "p", pch = 16,
    col = col_alpha(sex_col[target_sex], 0.7))
  for (i in 1:nrow(cfact)) {
    pr_cac_ind_mean <- rep(NA, length(ppl_tar))
    for (j in 1:length(ppl_tar)) {
      pr_cac <- logistic(samples$b * (cfact$age_su[i] - samples[,l_cols[ppl_tar[j]]]))
      pr_cac_ind_mean[j] <- mean(pr_cac)
    }
    cfact$pr_cac_mean[i] <- mean(pr_cac_ind_mean)
    cfact$pr_cac_lb[i] <- HPDI(pr_cac_ind_mean)[1]
    cfact$pr_cac_ub[i] <- HPDI(pr_cac_ind_mean)[2]
  }
  points(cfact$age, cfact$pr_cac_mean, type = "l", col = sex_col[target_sex], lwd = 2)
}

text(56, 0.75, labels = "\\textbf{Men}", col = sex_col[1], cex = 1.1)
text(72.8, 0.45, labels = "\\textbf{Women}", col = sex_col[2], cex = 1.1)

text(47.5, 0.95, labels = "(a)")

#############

plot(NULL,
  xlim = c(36, 55), ylim = c(3, 4.2),
  xlab = "Onset Age", ylab = "CAC Doubling Time (Years)",
  las = 1, xaxt = "n", yaxt = "n", frame.plot = TRUE
)
# what doubling time hits 100 CAC at age A for a given onset age x?
ages <- 27:85
age_cols <- viridis(length(ages))
# the two critical points in the polygon are:
for (i in ages) {
  growth_polygon(ages[i], age_cols[i])
#  Sys.sleep(0.1)
}

iso_slope <- doubling_point(47, 70) - doubling_point(46, 70)
axis_ratio <- (4.2-3)/(55-36)
iso_slope_true <- iso_slope/axis_ratio * 0.87
x65 <- 43
text(x65 + 0.5, doubling_point(x65, 65), "100 CAC by Age 65",
  srt = (360 / (2*pi)) * atan(iso_slope_true), col = "white", cex = 0.8)
x75 <- 50
text(x75 + 0.5, doubling_point(x75, 75), "100 CAC by Age 75",
  srt = (360 / (2*pi)) * atan(iso_slope_true), col = "white", cex = 0.8)

curve(doubling_point(x, 60), from = 0, to = 61, add = TRUE, col = "white", lwd = 0.1)
curve(doubling_point(x, 65), from = 0, to = 61, add = TRUE, col = "white", lwd = 0.1)
curve(doubling_point(x, 70), from = 0, to = 61, add = TRUE, col = "white", lwd = 0.1)
curve(doubling_point(x, 75), from = 0, to = 61, add = TRUE, col = "white", lwd = 0.1)
curve(doubling_point(x, 80), from = 0, to = 61, add = TRUE, col = "white", lwd = 0.1)

pos_code <- matrix(c(3, 4, 2, 4, 4, 3, 1, 4), byrow = TRUE, ncol = 4)

for (target_sex in 1:2) {
  for (target_eth in 1:4) {
    tar <- which(ppl$sex == target_sex & ppl$eth == target_eth & ppl$cac_obs)
    t0 <- apply(samples[,t0_cols[tar]], 1, mean)
    t0_mean <- mean(t0)
    d <- apply(samples[,d_cols[tar]], 1, mean)
    d_mean <- mean(d)
    points(t0_mean * 10 + 50, d_mean * 10, pch = 16,
      col = ifelse(target_sex == 2, "black", gray(0.3)))
    text(t0_mean * 10 + 50, d_mean * 10, labels = eth_labels[target_eth],
      pos = pos_code[target_sex, target_eth], col = ifelse(target_sex == 2, "black", gray(0.3)))
  }
  tar <- which(ppl$sex == target_sex & ppl$cac_obs)
  t0 <- apply(samples[,t0_cols[tar]], 1, mean)
  t0_mean <- mean(t0)
  d <- apply(samples[,d_cols[tar]], 1, mean)
  d_mean <- mean(d)
  points(t0_mean * 10 + 50, d_mean * 10, pch = 16,
         col = ifelse(target_sex == 2, "black", gray(0.3)), cex = 1.5)
  text(t0_mean * 10 + 50, d_mean * 10, pch = 16,
         col = ifelse(target_sex == 2, "black", gray(0.3)), 
         labels = ifelse(target_sex == 2, "\\textbf{Women}", "\\textbf{Men}"),
         cex = 1.1, pos = 2)
}

axis(2, at = seq(2.4, 5.0, by = 0.2), tck = 0.02, las = 1)
axis(2, at = seq(2.4, 5.0, by = 0.1), tck = 0.01, las = 1, labels = NA)
axis(1, at = seq(36, 55, by = 2), tck = 0.02)
axis(1, at = seq(37, 56, by = 1), tck = 0.01, labels = NA)
text(36.5, 4.15, labels = "(b)")

dev.off()

















print("make figure onsetGrowthSexInd")

# png("./figures/onsetGrowthSexInd.png", res = 300, units = "in", height = 5, width = 10, type = "cairo")
tikz("./figures/onsetGrowthSexInd.tex", height = 3.7, width = 7)

par(mfrow = c(1, 2))

for (target_sex in 1:2) {
  plot(NULL,
    xlim = c(0, 90), ylim = c(1, 6.5),
    xlab = "Patient Age", ylab = "CAC Doubling Time (Years)",
    las = 1, main = c("MESA Men", "MESA Women")[target_sex],
    xaxt = "n", yaxt = "n"
  )
  abline(v = seq(0, 80, by = 10), col = col_alpha("gray", 0.3))
  abline(h = seq(1, 12, by = 1), col = col_alpha("gray", 0.3))
  for (target_eth in 1:4) {
    tar <- which(ppl$sex == target_sex & ppl$eth == target_eth & ppl$cac_obs)
    t0_i_mean <- apply(samples[,t0_cols[tar]], 2, mean)
    d_i_mean <- apply(samples[,d_cols[tar]], 2, mean)
    points(t0_i_mean * 10 + 50, d_i_mean * 10, col = col_alpha(sex_col[target_sex], 0.3), pch = 16)
  }
  tar <- which(ppl$sex == target_sex & ppl$cac_obs)
  t0_i_mean <- apply(samples[,t0_cols[tar]], 2, mean)
  d_i_mean <- apply(samples[,d_cols[tar]], 2, mean)
  m_i_mean <- lm(d_i_mean ~ t0_i_mean)
  curve(10 * (coef(m_i_mean)[1] + coef(m_i_mean)[2] * (x - 50) / 10), from = 5, to = 80, add = TRUE)
  # which combos have >100 CAC by age 60?
  curve(10 * log(2)/log(100) * ((40-50)/10 - (x-50)/10), from = 0, to = 60, add = TRUE, lty = 2, col = gray(0.3, 0.5))
  curve(10 * log(2)/log(100) * ((50-50)/10 - (x-50)/10), from = 0, to = 60, add = TRUE, lty = 2, col = gray(0.3, 0.5))
  curve(10 * log(2)/log(100) * ((60-50)/10 - (x-50)/10), from = 0, to = 60, add = TRUE, lty = 2, col = gray(0.3, 0.5))
  curve(10 * log(2)/log(100) * ((70-50)/10 - (x-50)/10), from = 0, to = 70, add = TRUE, lty = 2, col = gray(0.3, 0.5))
  curve(10 * log(2)/log(100) * ((80-50)/10 - (x-50)/10), from = 0, to = 80, add = TRUE, lty = 2, col = gray(0.3, 0.5))
  curve(10 * log(2)/log(100) * ((90-50)/10 - (x-50)/10), from = 0, to = 90, add = TRUE, lty = 2, col = gray(0.3, 0.5))
  curve(10 * log(2)/log(100) * ((100-50)/10 - (x-50)/10), from = 0, to = 90, add = TRUE, lty = 2, col = gray(0.3, 0.5))
  axis(1, seq(0, 100, by=10), tck = -0.04)
  axis(1, seq(0, 100, by=5), tck = -0.02, labels = NA)
  axis(2, seq(1, 7, by=1), tck = -0.04, las = 1)
  axis(2, seq(1, 7, by=0.5), tck = -0.02, labels = NA)
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
abline(v = seq(40, 95, by = 5), col = gray(0.8))
abline(h = seq(0, 1, by = 0.1), col = gray(0.8))
for (target_sex in 1:2) {
  dm_tar <- which(dm$sex == target_sex)
  ppl_tar <- which(ppl$sex == target_sex)
  points(dm$age[dm_tar], dm$pr_cac[dm_tar], type = "p", pch = 16, col = sex_col[target_sex])
  for (i in 1:nrow(cfact)) {
    pr_cac_ind_mean <- rep(NA, length(ppl_tar))
    for (j in 1:length(ppl_tar)) {
      pr_cac <- logistic(samples$b * (cfact$age_su[i] - samples[,l_cols[ppl_tar[j]]]))
      pr_cac_ind_mean[j] <- mean(pr_cac)
    }
    cfact$pr_cac_mean[i] <- mean(pr_cac_ind_mean)
    cfact$pr_cac_lb[i] <- HPDI(pr_cac_ind_mean)[1]
    cfact$pr_cac_ub[i] <- HPDI(pr_cac_ind_mean)[2]
  }
  points(cfact$age, cfact$pr_cac_mean, type = "l", col = sex_col[target_sex])
}

text(56, 0.77, labels = "men", col = sex_col[1])
text(72.8, 0.44, labels = "women", col = sex_col[2])

dev.off()



print("make figure hasCacAgeSexEth")

png("./figures/hasCacAgeSexEth.png", res = 300, units = "in", height = 10, width = 10, type = "cairo")

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
    for (i in 1:nrow(cfact)) {
      pr_cac_ind_mean <- rep(NA, length(ppl_tar))
      for (j in 1:length(ppl_tar)) {
        pr_cac <- logistic(samples$b * (cfact$age_su[i] - samples[,l_cols[ppl_tar[j]]]))
        pr_cac_ind_mean[j] <- mean(pr_cac)
      }
      cfact$pr_cac_mean[i] <- mean(pr_cac_ind_mean)
      cfact$pr_cac_lb[i] <- HPDI(pr_cac_ind_mean)[1]
      cfact$pr_cac_ub[i] <- HPDI(pr_cac_ind_mean)[2]
    }
    points(cfact$age, cfact$pr_cac_mean, type = "l", col = sex_col[target_sex])
    text(56, 0.77, labels = "men", col = sex_col[1])
    text(72.8, 0.44, labels = "women", col = sex_col[2])
  }
}

dev.off()






print("make figure hasCacAge5Sex")

png("./figures/hasCacAge5Sex.png", res = 300, units = "in", height = 5, width = 5, type = "cairo")

dm <- obs_age5_sex

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
  points(dm$age5[dm_tar], dm$pr_cac[dm_tar], type = "p", pch = 16, col = sex_col[target_sex])
  for (i in 1:nrow(cfact)) {
    pr_cac_ind_mean <- rep(NA, length(ppl_tar))
    for (j in 1:length(ppl_tar)) {
      pr_cac <- logistic(samples$b * (cfact$age_su[i] - samples[,l_cols[ppl_tar[j]]]))
      pr_cac_ind_mean[j] <- mean(pr_cac)
    }
    cfact$pr_cac_mean[i] <- mean(pr_cac_ind_mean)
    cfact$pr_cac_lb[i] <- HPDI(pr_cac_ind_mean)[1]
    cfact$pr_cac_ub[i] <- HPDI(pr_cac_ind_mean)[2]
  }
  points(cfact$age, cfact$pr_cac_mean, type = "l", col = sex_col[target_sex])
}

text(56, 0.77, labels = "men", col = sex_col[1])
text(72.8, 0.44, labels = "women", col = sex_col[2])

dev.off()



print("make figure hasCacAge5SexEth")

png("./figures/hasCacAge5SexEth.png", res = 300, units = "in", height = 10, width = 10, type = "cairo")

dm <- obs_age5_sex_eth

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
    points(dm$age5[dm_tar], dm$pr_cac[dm_tar], type = "p", pch = 16, col = sex_col[target_sex])
    for (i in 1:nrow(cfact)) {
      pr_cac_ind_mean <- rep(NA, length(ppl_tar))
      for (j in 1:length(ppl_tar)) {
        pr_cac <- logistic(samples$b * (cfact$age_su[i] - samples[,l_cols[ppl_tar[j]]]))
        pr_cac_ind_mean[j] <- mean(pr_cac)
      }
      cfact$pr_cac_mean[i] <- mean(pr_cac_ind_mean)
      cfact$pr_cac_lb[i] <- HPDI(pr_cac_ind_mean)[1]
      cfact$pr_cac_ub[i] <- HPDI(pr_cac_ind_mean)[2]
    }
    points(cfact$age, cfact$pr_cac_mean, type = "l", col = sex_col[target_sex])
    text(56, 0.77, labels = "men", col = sex_col[1])
    text(72.8, 0.44, labels = "women", col = sex_col[2])
  }
}

dev.off()





print("make figure indCacAgeEthnicity")

png("./figures/indCacAgeEthnicity.png", res = 300, units = "in", height = 10, width = 10, type = "cairo")

par(mfrow = c(2, 2))

for (target_eth in 1:4) {
  plot(NULL,
    xlim = c(45, 90), ylim = c(1, 10000),
    xlab = "Patient Age", ylab = "CAC (Agatston Units)",
    main = paste0("CAC Magnitudes, N=5 MESA ", eth_labels[target_eth]),
    las = 1, log = "y"
  )
  sampled_ppl <- sample(ppl$pid[ppl$eth == target_eth & ppl$cac_obs], 5)
  sample_cols <- turbo(length(sampled_ppl))
  for (i in 1:length(sampled_ppl)) {
    points(obs$age[obs$pid == sampled_ppl[i]], obs$cac[obs$pid == sampled_ppl[i]], col = sample_cols[i], pch = 16)
    for (j in 1:nrow(cfact)) {
      my_exp_mu <- exp(samples[,k_cols[sampled_ppl[i]]] * (cfact$age_su[j] - samples[,t0_cols[sampled_ppl[i]]]))
      cfact$cac_median[j] <- median(my_exp_mu)
      cfact$cac_lb[j] <- HPDI(my_exp_mu)[1]
      cfact$cac_ub[j] <- HPDI(my_exp_mu)[2]
    }
    cfact$cac_median[cfact$cac_median < 1] <- NA
    points(cfact$age, cfact$cac_median, type = "l", col = sample_cols[i])
  }
}

dev.off()
