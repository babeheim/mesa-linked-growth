
rm(list = ls())

source("project_support.R")

############

sim_pars <- prior_pars
sim_pars$seed <- project_seed
sim_pars$N_ind <- n_ind

sim <- sim_cac_mesa(sim_pars)

save(sim, file = "./RData/sim.RData")

ppl <- sim$ppl
obs <- sim$obs

print(paste(nrow(ppl), "people,", nrow(obs), "observations simulated"))

# assign cross-validation bins, both for within-individual and between-individual prediction
ppl$between_cv_set <- assign_sets(nrow(ppl), n_cv_sets)
obs$between_cv_set <- ppl$between_cv_set[match(obs$pid, ppl$pid)]
ppl <- select(ppl, -between_cv_set)
obs$within_cv_set <- obs$between_cv_set
obs$within_cv_set[obs$exam == 1] <- 0

# check the columns

expect_true(all(c("pid", "eth", "sex", "age_exam1", "exam_schedule", "source") %in% colnames(ppl)))
expect_true(all(c("exam", "age", "age_su", "cac", "pid", "scan", "sex", "eth", "source", "between_cv_set", "within_cv_set") %in% colnames(obs)))

# write data to file

write.csv(obs, "observations.csv", row.names = FALSE)
write.csv(ppl, "people.csv", row.names = FALSE)

png("figures/simulation_dashboard.png", res = 300, units = "in", height = 10, width = 10, type = "cairo")

par(mfrow = c(2, 2))

plot(sim$l * 10 + 50, sim$d * 10, col = eth_col[ppl$eth], pch = ifelse(ppl$sex == 1, 1, 16))

plot(sim$t0 * 10 + 50, sim$d * 10, col = eth_col[ppl$eth], pch = ifelse(ppl$sex == 1, 1, 16))

plot(NULL, xlim = c(-5, 5), ylim = c(0, 1))
for (i in 1:length(sim$l)) {
  curve(logistic(sim$b * (x - sim$l[i])), add = TRUE, col = col_alpha("black", 0.2))
}

plot(NULL, xlim = c(-5, 5), ylim = c(0, 10))
for (i in 1:length(sim$l)) {
  curve(sim$k[i] * (x - sim$t0[i]), add = TRUE, col = col_alpha(eth_col[ppl$eth[i]], 0.2),
  from = c(sim$t0[i], 5))
}

dev.off()




