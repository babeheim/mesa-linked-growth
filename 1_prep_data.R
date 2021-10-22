
rm(list = ls())

source("project_support.R")

## add Exam 1

d1 <- read.csv("raw_data/MESA_2020b/Primary/Exam1/Data/mesae1dres06192012.csv")

expect_true(mean(d1$age1c) %>% near(62.2, 0.1))
expect_true(sum(is.na(d1$agatp11c)) == 1)
expect_true(sum(is.na(d1$agatp21c)) == 69)
expect_true(sum(d1$agatp11c == 0, na.rm = TRUE) == 3563)
expect_true(sum(d1$agatp21c == 0, na.rm = TRUE) == 3519)
expect_true(mean(d1$agatp11c, na.rm = TRUE) %>% near(146.5, 0.1))
expect_true(mean(d1$agatp21c, na.rm = TRUE) %>% near(145.7, 0.1))

dat <- data.frame(
  exam = 1,
  scan = 1,
  mesa_id = d1$MESAID,
  age = d1$age1c,
  cac = d1$agatp11c
)
add <- data.frame(
  exam = 1,
  scan = 2,
  mesa_id = d1$MESAID,
  age = d1$age1c,
  cac = d1$agatp21c
)

obs_exam1 <- bind_rows(dat, add)
obs <- obs_exam1

print("exam 1 added")

## add Exam 2

d2 <- read.csv("raw_data/MESA_2020b/Primary/Exam2/Data/mesae2dres06222012.csv")

expect_true(all(d2$mesaid %in% d1$MESAID))
expect_true(mean(d2$age2c, na.rm = TRUE) %>% near(63.6, 0.1))
expect_true(sum(is.na(d2$agatp12c)) == 3861)
expect_true(sum(is.na(d2$agatp22c)) == 3874)
expect_true(sum(d2$agatp12c == 0, na.rm = TRUE) == 1444)
expect_true(sum(d2$agatp22c == 0, na.rm = TRUE) == 1456)
expect_true(mean(d2$agatp22c, na.rm = TRUE) %>% near(170.3, 0.1))

dat <- data.frame(
  exam = 2,
  scan = 1,
  mesa_id = d2$mesaid,
  age = d2$age2c,
  cac = d2$agatp12c
)
add <- data.frame(
  exam = 2,
  scan = 2,
  mesa_id = d2$mesaid,
  age = d2$age2c,
  cac = d2$agatp22c
)

obs_exam2 <- bind_rows(dat, add)
obs <- bind_rows(obs, obs_exam2)

print("exam 2 added")

## add Exam 3

d3 <- read.csv("raw_data/MESA_2020b/Primary/Exam3/Data/mesae3dres06222012.csv")

expect_true(all(d3$mesaid %in% d1$MESAID))

dat <- data.frame(
  exam = 3,
  scan = 1,
  mesa_id = d3$mesaid,
  age = d3$age3c,
  cac = d3$agatp13c
)
add <- data.frame(
  exam = 3,
  scan = 2,
  mesa_id = d3$mesaid,
  age = d3$age3c,
  cac = d3$agatp23c
)

obs_exam3 <- bind_rows(dat, add)
obs <- bind_rows(obs, obs_exam3)

print("exam 3 added")

## add Exam 4

d4 <- read.csv("raw_data/MESA_2020b/Primary/Exam4/Data/mesae4dres06222012.csv")

expect_true(all(d4$mesaid %in% d1$MESAID))

dat <- data.frame(
  exam = 4,
  scan = 1,
  mesa_id = d4$mesaid,
  age = d4$age4c,
  cac = d4$agatp14c
)
add <- data.frame(
  exam = 4,
  scan = 2,
  mesa_id = d4$mesaid,
  age = d4$age4c,
  cac = d4$agatp24c
)

obs_exam4 <- bind_rows(dat, add)
obs <- bind_rows(obs, obs_exam4)

print("exam 4 added")

## add Exam 5

d5 <- read.csv("raw_data/MESA_2020b/Primary/Exam5/Data/mesae5_drepos_20151101.csv")

expect_true(all(d5$mesaid %in% d1$MESAID))

dat <- data.frame(
  exam = 5,
  scan = 1,
  mesa_id = d5$mesaid,
  age = d5$age5c,
  cac = d5$agatpm5c
)

obs_exam5 <- dat
obs <- bind_rows(obs, obs_exam5)

print("exam 5 added")

# create people table

ppl <- data.frame(
  mesa_id = d1$MESAID,
  male = d1$gender1,
  eth = d1$race1c,
  # coverage flags
  in_exam1 = 1,
  in_exam2 = as.numeric(d1$MESAID %in% obs_exam2$mesa_id[!is.na(obs_exam2$cac)]),
  in_exam3 = as.numeric(d1$MESAID %in% obs_exam3$mesa_id[!is.na(obs_exam3$cac)]),
  in_exam4 = as.numeric(d1$MESAID %in% obs_exam4$mesa_id[!is.na(obs_exam4$cac)]),
  in_exam5 = as.numeric(d1$MESAID %in% obs_exam5$mesa_id[!is.na(obs_exam5$cac)])
)

ppl$age_exam1 <- d1$age1c[match(ppl$mesa_id, d1$MESAID)]

print("people table constructed")

expect_true(!any(is.na(obs$age) & !is.na(obs$cac)))
# in cases where age is missing, CAC is also missing

# drop empty rows, unneeded variables

obs <- subset(obs, !is.na(age) & !is.na(cac))

# completeness checks

expect_true(nrow(ppl) == 6814)
expect_true(nrow(obs) == 31148)
expect_true(all(obs$pid %in% ppl$pid))
expect_true(!any(is.na(ppl$age_exam1)))
expect_true(!any(is.na(ppl$male)))
expect_true(!any(is.na(ppl$eth)))

# sample individuals from full dataset + reassign pids

keep <- sample(1:nrow(ppl), n_ind)
ppl <- ppl[keep,]

keep_obs <- which(obs$mesa_id %in% ppl$mesa_id)
obs <- obs[keep_obs,]

ppl$pid <- 1:nrow(ppl)
obs$pid <- match(obs$mesa_id, ppl$mesa_id)

expect_true(all(ppl$mesa_id[5] == obs$mesa_id[obs$pid == 5]))
expect_true(all(ppl$mesa_id[15] == obs$mesa_id[obs$pid == 15]))
expect_true(all(ppl$mesa_id[25] == obs$mesa_id[obs$pid == 25]))

# create derived variables for analysis

ppl$exam_schedule <- NA
for (i in 1:nrow(ppl)) {
  ppl$exam_schedule[i] <- paste0(sort(unique(obs$exam[obs$pid == ppl$pid[i]])), collapse = "")
}

obs$age_su <- (obs$age - 50) / 10
ppl$sex <- 2 - ppl$male # 1 = male, 2 = female
obs$eth <- ppl$eth[match(obs$pid, ppl$pid)]
obs$sex <- ppl$sex[match(obs$pid, ppl$pid)]

# filter to just the variables we need
ppl <- select(ppl, pid, sex, eth, age_exam1, exam_schedule)
obs <- select(obs, pid, exam, eth, sex, scan, age, age_su, cac)

# assign cross-validation bins, both for within-individual and between-individual prediction
ppl$between_cv_set <- assign_sets(nrow(ppl), n_cv_sets)
obs$between_cv_set <- ppl$between_cv_set[match(obs$pid, ppl$pid)]
ppl <- select(ppl, -between_cv_set)
obs$within_cv_set <- obs$between_cv_set
obs$within_cv_set[obs$exam == 1] <- 0

# final integrity checks

expect_true(nrow(ppl) == n_ind)
expect_true(all(obs$pid %in% ppl$pid))
expect_true(!any(is.na(ppl$age_exam1)))
expect_true(!any(is.na(ppl$sex)))
expect_true(!any(is.na(ppl$eth)))
expect_true(!any(is.na(obs$age)))
expect_true(!any(is.na(obs$age_su)))
expect_true(all(obs$exam %in% 1:5))
expect_true(!any(is.na(obs$cac)))
expect_true(all(obs$between_cv_set %in% 1:n_cv_sets))
expect_true(all(obs$within_cv_set %in% 0:n_cv_sets))

# tag the data provenance

ppl$source <- "mesa sample"
obs$source <- "mesa sample"

# check the columns

expect_true(all(c("pid", "eth", "sex", "age_exam1", "exam_schedule", "source") %in% colnames(ppl)))
expect_true(all(c("exam", "age", "age_su", "cac", "pid", "scan", "sex", "eth", "source", "between_cv_set", "within_cv_set") %in% colnames(obs)))

# write data to file

write.csv(obs, "observations.csv", row.names = FALSE)
write.csv(ppl, "people.csv", row.names = FALSE)
