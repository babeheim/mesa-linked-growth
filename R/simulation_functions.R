
diag_matrix <- function(v) {
  out <- diag(length(v))
  diag(out) <- v
  return(out)
}

quad_form_diag <- function(Omega, tau) {
  diag_matrix(tau) %*% Omega %*% diag_matrix(tau)
}

sim_cac_mesa <- function(pars) {

  if (!hasName(pars, "seed")) stop("seed must be NA or specified")
  if (!hasName(pars, "N_ind") |
      !hasName(pars, "l_pop_mu") |
      !hasName(pars, "l_pop_sigma") |
      !hasName(pars, "l_ind_sigma_mu") |
      !hasName(pars, "l_sex_sigma_mu") |
      !hasName(pars, "l_eth_sigma_mu") |
      !hasName(pars, "lk_eth_eta") |
      !hasName(pars, "lk_ind_eta") |
      !hasName(pars, "b_mu") |
      !hasName(pars, "b_sigma") |
      !hasName(pars, "k_pop_mu") |
      !hasName(pars, "k_pop_sigma") |
      !hasName(pars, "k_ind_sigma_mu") |
      !hasName(pars, "k_sex_sigma_mu") |
      !hasName(pars, "k_eth_sigma_mu") |
      !hasName(pars, "s_mu")) stop("essential parameters are missing!")

  # check constraints of parameters
  if (pars$N_ind <= 0 | pars$N_ind %% 1 != 0) stop("invalid N_ind")
  if (pars$b_mu > 20) stop("precision on t0 is too high")

  # generate growth variables
  out <- list()

  out$seed <- pars$seed
  if (is.na(out$seed)) out$seed <- sample(1:1000, 1)
  set.seed(out$seed)

  out$l_pop <- rnorm(1, pars$l_pop_mu, pars$l_pop_sigma)
  out$l_ind_sigma <- rexp(1, 1/pars$l_ind_sigma_mu)
  out$l_sex_sigma <- rexp(1, 1/pars$l_sex_sigma_mu)
  out$l_eth_sigma <- rexp(1, 1/pars$l_eth_sigma_mu)

  out$k_pop <- rnorm(1, pars$k_pop_mu, pars$k_pop_sigma)
  out$k_ind_sigma <- rexp(1, 1/pars$k_ind_sigma_mu)
  out$k_sex_sigma <- rexp(1, 1/pars$k_sex_sigma_mu)
  out$k_eth_sigma <- rexp(1, 1/pars$k_eth_sigma_mu)

  out$b <- rlnorm(1, pars$b_mu, pars$b_sigma)
  out$v <- 1/out$b

  out$lk_eth_rho <- 2 * rbeta(1, pars$lk_eth_eta, pars$lk_eth_eta) - 1
  out$lk_eth_tau <- c(out$l_eth_sigma, out$k_eth_sigma) # l first, then k
  out$lk_eth_cor <- matrix(c(1, out$lk_eth_rho, out$lk_eth_rho, 1), ncol = 2, byrow = TRUE)
  out$lk_eth_cov <- quad_form_diag(out$lk_eth_cor, out$lk_eth_tau)
  out$lk_eth <- rmvnorm(4, c(0, 0), out$lk_eth_cov)

  out$l_sex <- rnorm(2, 0, out$l_sex_sigma)
  out$l_eth <- out$lk_eth[,1]
  out$l_ind <- rep(NA, pars$N_ind)
  out$l <- rep(NA, pars$N_ind)

  out$t0 <- rep(NA, pars$N_ind)

  out$k_sex <- rnorm(2, 0, out$k_sex_sigma)
  out$k_eth <- out$lk_eth[,2]
  out$k_ind <- rep(NA, pars$N_ind)
  out$k <- rep(NA, pars$N_ind)

  out$lk_ind_rho <- 2 * rbeta(1, pars$lk_ind_eta, pars$lk_ind_eta) - 1
  out$lk_ind_tau <- c(out$l_ind_sigma, out$k_ind_sigma) # l first, then k
  out$lk_ind_cor <- matrix(c(1, out$lk_ind_rho, out$lk_ind_rho, 1), ncol = 2, byrow = TRUE)
  out$lk_ind_cov <- quad_form_diag(out$lk_ind_cor, out$lk_ind_tau)

  out$s <- rexp(1, 1/pars$s_mu)

  sim <- list()

  for (i in 1:pars$N_ind) {
    # initialize person
    dat <- list(
      pid = i,
      sex = sample(1:2, 1), # where 1 = male, 2 = female
      age_exam1 = (2000 - sample(1916:1955, 1, prob = c(11:30, rep(20, 20)))), # weights to resemble MESA sample
      eth = sample(1:4, 1, prob = c(4, 3, 2, 1)) # weights to resemble four MESA ethnicities (white, black, hispanic, asian)
    )

    # set individual-specific parameters
    lk_ind <- rmvnorm(1, c(0, 0), out$lk_ind_cov)
    out$l_ind[i] <- lk_ind[1]
    out$l[i] <- out$l_pop +
      out$l_sex[dat$sex] +
      out$l_eth[dat$eth] +
      out$l_ind[i]
    # assumption: no one can have an l before being born
    if (out$l[i] < (-5)) out$l[i] <- (-5)
    out$t0[i] <- rlogis(1, out$l[i], out$v)
    # assumption: no one can have a t0 before being born
    if (out$t0[i] < (-5)) out$t0[i] <- (-5)
    out$k_ind[i] <- lk_ind[2]
    out$k[i] <- exp(out$k_pop +
      out$k_sex[dat$sex] +
      out$k_eth[dat$eth] +
      out$k_ind[i])

    # select MESA-like study design for each person
    exam_years <- c(2000, 2002, 2004, 2006, 2010)
    designs <- c("135", "12", "13", "1245", "1", "125", "124", "145", "14", "1345", "134", "15")
    design_weights <- c(252, 187, 151, 110, 109, 091, 045, 024, 018, 005, 003, 003) # actual MESA weights
    dat$exam_schedule <- sample(designs, 1, replace = TRUE, prob = design_weights)
    dat$exams <- as.numeric(strsplit(dat$exam_schedule, split = "")[[1]])
    dat$exam_years <- exam_years[dat$exams]
    dat$exam_ages <- dat$exam_years - (2000 - dat$age_exam1)
    dat$age_su = (dat$exam_ages - 50)/10

    # calculate cac for exam ages
    dat$cac_true <- rep(0, length(dat$age_su))
    dat$cac_scan1 <- rep(0, length(dat$age_su))
    dat$cac_scan2 <- rep(0, length(dat$age_su))
    dat$growth_rates <- rep(NA, length(dat$age_su))
    growing <- which(dat$age_su >= out$t0[i])
    if (length(growing) > 0) {
      dat$growth_rates[growing] <- rep(out$k[i], length(growing))
      dat$cac_true[growing] <- exp(dat$growth_rates[growing] * (dat$age_su[growing] - out$t0[i]))
      dat$cac_scan1[growing] <- rlnorm(length(growing), dat$growth_rates[growing] * (dat$age_su[growing] - out$t0[i]), out$s)
      dat$cac_scan2[growing] <- rlnorm(length(growing), dat$growth_rates[growing] * (dat$age_su[growing] - out$t0[i]), out$s)
    }
    if (any(dat$cac_true == "Inf")) stop("infinite CAC growth!")
    # record results
    sim[[i]] <- dat
  }

  # extract person info from sim
  ppl <- list()
  for (i in 1:pars$N_ind) ppl[[i]] <- sim[[i]][c("pid", "sex", "age_exam1", "exam_schedule", "eth")]
  ppl %>% bind_rows() %>% as.data.frame() -> ppl

  # extract observation info from sim
  obs <- list()
  for (i in 1:pars$N_ind) {
    obs[[i]] <- sim[[i]][c("exams", "exam_ages", "age_su", "growth_rates", "cac_true", "cac_scan1", "cac_scan2")]
    obs[[i]]$pid <- rep(sim[[i]]$pid, length(sim[[i]]$exam_ages))
  }
  obs %>% bind_rows() %>% as.data.frame() -> obs

  obs <- rename(obs,
    growth_rate = growth_rates,
    exam = exams,
    age = exam_ages
  )

  # for some reason, MESA exam 5 doesn't have two scans
  if (any(obs$exam == 5)) {
    obs$cac_scan2[obs$exam == 5] <- NA
  }

  # now make the two cac scans long-form
  obs_scan1 <- rename(obs, cac = cac_scan1)
  obs_scan1 <- select(obs_scan1, -cac_scan2)
  obs_scan1$scan <- 1
  obs_scan2 <- rename(obs, cac = cac_scan2)
  obs_scan2 <- select(obs_scan2, -cac_scan1)
  obs_scan2$scan <- 2
  obs <- bind_rows(obs_scan1, obs_scan2)

  obs <- subset(obs, !is.na(obs$cac))
  o <- order(obs$pid, obs$exam)
  obs <- obs[o,]

  # generated quantities
  out$a <- -(out$l * out$b)
  out$m <- -(out$t0 * out$k)
  out$d <- log(2)/out$k

  ppl$cac_start_age <- out$t0 * 10 + 50
  obs$sex <- ppl$sex[match(obs$pid, ppl$pid)]
  obs$eth <- ppl$eth[match(obs$pid, ppl$pid)]

  # tag the data provenance
  ppl$source <- "simulation"
  obs$source <- "simulation"

  out <- c(
    out,
    list(
      ppl = ppl,
      obs = obs
    )
  )
  return(out)

}
