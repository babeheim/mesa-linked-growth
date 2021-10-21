
extract_diagnostics <- function(cmdstan_fit) {

  out <- list()

  diag <- cmdstan_fit$cmdstan_diagnose()$stdout
  out$rhat_good <- grepl("Split R-hat values satisfactory all parameters.", diag)
  out$ess_good <- grepl("Effective sample size satisfactory.", diag)
  out$energy_good <- grepl("E-BFMI satisfactory for all transitions.", diag)
  out$divergence_good <- grepl("No divergent transitions found.", diag)

  x <- cmdstan_fit$cmdstan_summary()$stdout
  pattern <- "Sampling took.*\\n"
  m <- gregexpr(pattern, x, perl = TRUE)
  sampling_time <- regmatches(x, m)[[1]]
  sampling_time <- gsub("\\n$", "", sampling_time)

  s <- sampling_time
  s <- gsub("Sampling .*, ", "", s)
  s <- gsub(" total", "", s)
  units <- gsub("\\d.*\\s", "", s)
  s <- as.numeric(gsub("\\s.*$", "", s))
  tar <- which(units  ==  "seconds")
  if (length(tar) > 0) s[tar] <- s[tar] / 60
  tar <- which(units  ==  "hours")
  if (length(tar) > 0) s[tar] <- s[tar] * 60
  out$sampling_time_min <- round(s, 1)

  return(out)
}

calc_group_variance_ratio <- function(x, g) {
  n_ind <- length(x)
  n_ind_g <- table(g)
  group_variances <- tapply(x, g, evar)
  group_means <- tapply(x, g, mean)
  group_e_v <- sum(n_ind_g * group_variances) / n_ind
  group_v_e <- sum(n_ind_g * (group_means - mean(group_means))^2) / n_ind
  out <- group_v_e / (group_e_v + group_v_e)
  return(out)
}

evar <- function(x) mean((x - mean(x))^2)

summarize_samples <- function(samples) {
  out <- list(
    mean = mean(samples),
    sd = sd(samples),
    lb = HPDI(samples)[1],
    ub = HPDI(samples)[2],
    psign = psign(samples)
  )
  return(out)
}

psign <- function(samples, n_digits = 3) {
  if (all(samples == 0)) stop("all samples are zero!")
  if (mean(samples) > 0) output <- round(mean(samples < 0), n_digits)
  if (mean(samples) < 0) output <- round(mean(samples > 0), n_digits)
  float_digits <- paste0("%.", n_digits, "f")
  output <- sprintf(float_digits, output)
  null_entry <- paste0("0.", paste0(rep("0", n_digits), collapse = ""))
  output[output  ==  null_entry] <- paste0("$<0.", paste0(rep("0", n_digits - 1), collapse = ""),"1$")
  return(output)
}

calc_sim_estimands <- function(sim) {

  out <- list()

  print("H1P1 - men have faster growth than women")
  out$h1p1e1 <- sim$k_sex[1] - sim$k_sex[2]
  out$h1p1e2 <- mean(sim$k[sim$ppl$sex == 1]) - mean(sim$k[sim$ppl$sex == 2])
  out$h1p1e3 <- mean(sim$d[sim$ppl$sex == 1]) - mean(sim$d[sim$ppl$sex == 2])
  
  print("H1P2 - distinct ethnic variation in growth rates")
  out$h1p2e1 <- sim$k_eth[1]
  out$h1p2e2 <- sim$k_eth[2]
  out$h1p2e3 <- sim$k_eth[3]
  out$h1p2e4 <- sim$k_eth[4]
  out$h1p2e5 <- sim$k_eth_sigma
  
  print("H2P1 - much more individual variation in onset than in growth")
  out$h2p1e1 <- 2 * (log(sim$l_ind_sigma) - log( sim$k_ind_sigma)) - log(25)
  out$h2p1e2 <- log(var(sim$l_ind) / var(sim$k_ind)) - log(25)

  print("H2P2 - much more total variation in onset than in growth")
  out$h2p2e1 <- log(var(sim$l) / var(sim$k)) - log(25)

  print("H3P1 - more inter-individual variation than between ethnicities on growth")
  out$h3p1e1 <- calc_group_variance_ratio(sim$d, sim$ppl$eth)
  out$h3p1e2 <- calc_group_variance_ratio(sim$k, sim$ppl$eth)
  
  print("H3P2 - more inter-individual variation than between ethnicities in age of onset")
  out$h3p2e1 <- calc_group_variance_ratio(sim$l, sim$ppl$eth)
  out$h3p2e2 <- calc_group_variance_ratio(sim$t0, sim$ppl$eth)

  print("H3P3 - more inter-individual variation than variation between sexes in growth")
  out$h3p3e1 <- calc_group_variance_ratio(sim$d, sim$ppl$sex)
  out$h3p3e2 <- calc_group_variance_ratio(sim$k, sim$ppl$sex)
  
  print("H3P4 - more inter-individual variation than variation between sexes in onset")
  out$h3p4e1 <- calc_group_variance_ratio(sim$l, sim$ppl$sex)

  print("H4P1 - correlation between age of onset and progression is negative by individual")
  out$h4p1e1 <- cor(sim$k, sim$l)
  out$h4p1e2 <- cor(sim$k_ind, sim$l_ind)
  out$h4p1e3 <- sim$lk_ind_rho

  return(out)

}

calc_key_estimates <- function(fit, data) {

  samples <- as.data.frame(as_draws_df(fit$draws()))

  calcs <- list()

  print("H1P1 - men have faster growth than women")

  if (any(grepl("k_sex", colnames(samples)))) {
    h1p1e1 <- samples$`k_sex[1]` - samples$`k_sex[2]`
    calcs$h1p1e1 <- summarize_samples(h1p1e1)
    calcs$h1p1e1$label <- "growthRateSexDiffAlt"
  }

  if (any(grepl("^k\\[", colnames(samples)))) {
    k_cols <- grep("^k\\[", colnames(samples))
    tar_male <- k_cols[data$sex == 1]
    tar_female <- k_cols[data$sex == 2]
    h1p1e2 <- apply(samples[,tar_male], 1, mean) - apply(samples[,tar_female], 1, mean)
    calcs$h1p1e2 <- summarize_samples(h1p1e2)
    calcs$h1p1e2$label <- "growthRateSexDiff"
  }

  if (any(grepl("^d\\[", colnames(samples)))) {
    d_cols <- grep("^d\\[", colnames(samples))
    tar_male <- d_cols[data$sex == 1]
    tar_female <- d_cols[data$sex == 2]
    h1p1e3 <- apply(samples[,tar_male], 1, mean) - apply(samples[,tar_female], 1, mean)
    calcs$h1p1e3 <- summarize_samples(h1p1e3)
    calcs$h1p1e3$label <- "doublingTimeSexDiff"
  }

  print("H1P2 - distinct ethnic variation in growth rates")

  if (any(grepl("^k\\[", colnames(samples)))) {
    k_cols <- grep("^k\\[", colnames(samples))
    k_mean <- apply(samples[,k_cols], 1, mean)
    tar <- which(ppl$eth == 1)
    calcs$h1p2e1 <- summarize_samples(apply(samples[,k_cols[tar]], 1, mean) - k_mean)
    calcs$h1p2e1$label <- "growthRateWhiteOffset"
    tar <- which(ppl$eth == 2)
    calcs$h1p2e2 <- summarize_samples(apply(samples[,k_cols[tar]], 1, mean) - k_mean)
    calcs$h1p2e2$label <- "growthRateAsianOffset"
    tar <- which(ppl$eth == 3)
    calcs$h1p2e3 <- summarize_samples(apply(samples[,k_cols[tar]], 1, mean) - k_mean)
    calcs$h1p2e3$label <- "growthRateBlackOffset"
    tar <- which(ppl$eth == 4)
    calcs$h1p2e4 <- summarize_samples(apply(samples[,k_cols[tar]], 1, mean) - k_mean)
    calcs$h1p2e4$label <- "growthRateHispanicOffset"
  }

  print("H2P1 - much more individual variation in onset than in growth")

  if ("l_ind_sigma" %in% colnames(samples) & "k_ind_sigma" %in% colnames(samples)) {
    calcs$h2p1e1 <- summarize_samples(2 * (log(samples$l_ind_sigma) - log(samples$k_ind_sigma)) - log(25))
    calcs$h2p1e1$label = "growthOnsetIndVarianceRatio"
  }

  print("H2P2 - much more total variation in onset than in growth")

  if (any(grepl("^k\\[", colnames(samples))) & any(grepl("^l\\[", colnames(samples)))) {
    k_cols <- grep("^k\\[", colnames(samples))
    l_cols <- grep("^l\\[", colnames(samples))
    calcs$h2p2e1 <- summarize_samples(2 * log(apply(samples[,k_cols], 1, var)) - log(apply(samples[,k_cols], 1, var)) - log(25))
    calcs$h2p2e1$label = "growthOnsetTotalVarianceRatio"
  }

  print("H3P1 - more inter-individual variation than between ethnicities on growth")

  if (any(grepl("^d\\[", colnames(samples)))) {
    d_cols <- grep("^d\\[", colnames(samples))
    eth_var_ratio <- apply(samples[,d_cols], 1, function(z) calc_group_variance_ratio(z, data$eth))
    calcs$h3p1e1 <- summarize_samples(eth_var_ratio)
    calcs$h3p1e1$label = "growthEthVarianceRatio"
  }

  print("H3P2 - more inter-individual variation than between ethnicities in age of onset")

  if (any(grepl("^l\\[", colnames(samples)))) {
    l_cols <- grep("^l\\[", colnames(samples))
    eth_var_ratio <- apply(samples[,l_cols], 1, function(z) calc_group_variance_ratio(z, data$eth))
    calcs$h3p2e1 <- summarize_samples(eth_var_ratio)
    calcs$h3p2e1$label = "onsetEthVarianceRatio"
  }

  print("H3P3 - more inter-individual variation than variation between sexes in growth")

  if (any(grepl("^d\\[", colnames(samples)))) {
    d_cols <- grep("^d\\[", colnames(samples))
    sex_var_ratio <- apply(samples[,d_cols], 1, function(z) calc_group_variance_ratio(z, data$sex))
    calcs$h3p3e1 <- summarize_samples(sex_var_ratio)
    calcs$h3p3e1$label = "growthSexVarianceRatio"
  }

  print("H3P4 - more inter-individual variation than variation between sexes in onset")

  if (any(grepl("^l\\[", colnames(samples)))) {
    l_cols <- grep("^l\\[", colnames(samples))
    sex_var_ratio <- apply(samples[,l_cols], 1, function(z) calc_group_variance_ratio(z, data$sex))
    calcs$h3p4e1 <- summarize_samples(sex_var_ratio)
    calcs$h3p4e1$label = "onsetSexVarianceRatio"
  }

  print("H4P1 - correlation between age of onset and progression is negative by individual")

  if (any(grepl("^l\\[", colnames(samples))) & any(grepl("^k\\[", colnames(samples)))) {
    l_cols <- grep("^l\\[", colnames(samples))
    k_cols <- grep("^k\\[", colnames(samples))
    lk_cor <- rep(NA, nrow(samples))
    for (i in 1:nrow(samples)) {
      lk_cor[i] <- cor(as.numeric(samples[i,k_cols]), as.numeric(samples[i,l_cols]))
    }
    calcs$h4p1e1 <- summarize_samples(lk_cor)
    calcs$h4p1e1$label <- "onsetGrowthCorr"
  }

  if (any(grepl("^lk_ind\\[", colnames(samples)))) {
    l_ind_cols <- grep("^lk_ind\\[\\d+,1\\]", colnames(samples))
    k_ind_cols <- grep("^lk_ind\\[\\d+,2\\]", colnames(samples))
    lk_cor <- rep(NA, nrow(samples))
    for (i in 1:nrow(samples)) {
      lk_cor[i] <- cor(as.numeric(samples[i,k_ind_cols]), as.numeric(samples[i,l_ind_cols]))
    }
    calcs$h4p1e2 <- summarize_samples(lk_cor)
    calcs$h4p1e2$label <- "onsetGrowthCorrExpl"
  }

  if ("lk_ind_rho" %in% colnames(samples)) {
    calcs$h4p1e3 <- summarize_samples(samples$lk_ind_rho)
    calcs$h4p1e3$label <- "onsetGrowthCorrAlt"
  }

  # its not clear we can actually detect this, with only a few observations per person...

  return(calcs)

}
