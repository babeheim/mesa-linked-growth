data {
  int<lower=1> N_ind;
  int sex[N_ind];
  int eth[N_ind];
  // training set
  int<lower=0> N_obs;
  int pid[N_obs];
  real age_su[N_obs];
  vector<lower=0>[N_obs] y;
  // validation set
  int<lower=0> N_obs_v;
  int pid_v[N_obs_v];
  real age_su_v[N_obs_v];
  vector<lower=0>[N_obs_v] y_v;
  // add priors as input
  real l_pop_mu;
  real<lower=0> l_pop_sigma;
  real<lower=0> l_ind_sigma_mu;
  real<lower=0> l_eth_sigma_mu;
  real<lower=0> l_sex_sigma_mu;
  real k_pop_mu;
  real<lower=0> k_pop_sigma;
  real<lower=0> k_ind_sigma_mu;
  real<lower=0> k_eth_sigma_mu;
  real<lower=0> k_sex_sigma_mu;
  real<lower=0> s_mu;
}
transformed data {
  // the log-linear model will just use the l priors for t0
  real t0_pop_mu = l_pop_mu;
  real<lower=0> t0_pop_sigma = l_pop_sigma;
  real<lower=0> t0_ind_sigma_mu = l_ind_sigma_mu;
  real<lower=0> t0_eth_sigma_mu = l_eth_sigma_mu;
  real<lower=0> t0_sex_sigma_mu = l_sex_sigma_mu;
  // its easier to think in terms of 1/rate for priors
  real<lower=0> t0_ind_sigma_rate = 1/t0_ind_sigma_mu;
  real<lower=0> t0_eth_sigma_rate = 1/t0_eth_sigma_mu;
  real<lower=0> t0_sex_sigma_rate = 1/t0_sex_sigma_mu;
  real<lower=0> k_ind_sigma_rate = 1/k_ind_sigma_mu;
  real<lower=0> k_eth_sigma_rate = 1/k_eth_sigma_mu;
  real<lower=0> k_sex_sigma_rate = 1/k_sex_sigma_mu;
  real<lower=0> s_rate = 1/s_mu;
}
parameters {
  real t0_pop;
  real<lower=0> t0_sex_sigma;
  vector[2] t0_sex;
  real<lower=0> t0_eth_sigma;
  vector[4] t0_eth;
  real<lower=0> t0_ind_sigma;
  vector[N_ind] t0_ind;
  real k_pop;
  real<lower=0> k_sex_sigma;
  vector[2] k_sex;
  real<lower=0> k_eth_sigma;
  vector[4] k_eth;
  real<lower=0> k_ind_sigma;
  vector[N_ind] k_ind;
  real<lower=0> s;
}
transformed parameters{
  vector[N_ind] t0 = t0_pop +
    t0_sex[sex] * t0_sex_sigma +
    t0_eth[eth] * t0_eth_sigma +
    t0_ind * t0_ind_sigma;
  vector[N_ind] k = exp(k_pop +
    k_sex[sex] * k_sex_sigma +
    k_eth[eth] * k_eth_sigma +
    k_ind * k_ind_sigma);
  vector[N_ind] m = -(t0 .* k);
}
model {
  t0_pop ~ normal(t0_pop_mu, t0_pop_sigma);
  t0_ind_sigma ~ exponential(t0_ind_sigma_rate);
  t0_ind ~ std_normal();
  t0_eth_sigma ~ exponential(t0_eth_sigma_rate);
  t0_eth ~ std_normal();
  t0_sex_sigma ~ exponential(t0_sex_sigma_rate);
  t0_sex ~ std_normal();
  k_pop ~ normal(k_pop_mu, k_pop_sigma);
  k_ind_sigma ~ exponential(k_ind_sigma_rate);
  k_ind ~ std_normal();
  k_eth_sigma ~ exponential(k_eth_sigma_rate);
  k_eth ~ std_normal();
  k_sex_sigma ~ exponential(k_sex_sigma_rate);
  k_sex ~ std_normal();
  s ~ exponential(s_rate);
  for (i in 1:N_obs) {
    if (y[i] != 0) {
      real mu = m[pid[i]] + k[pid[i]] * age_su[i];
      target += lognormal_lpdf(y[i] | mu, s);
    }
  }
}
generated quantities {
  vector[N_ind] d = log(2) ./ k;
  real lognormal_vll = 0;
  if (N_obs_v > 0) {
    for (i in 1:N_obs_v) {
      if (y_v[i] != 0) {
        real mu_pred = m[pid_v[i]] + k[pid_v[i]] * age_su_v[i];
        lognormal_vll += lognormal_lpdf(y_v[i] | mu_pred, s);
      }
    }
  }
}
