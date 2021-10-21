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
  real b_mu;
  real<lower=0> b_sigma;
  real k_pop_mu;
  real<lower=0> k_pop_sigma;
  real<lower=0> k_ind_sigma_mu;
  real<lower=0> k_eth_sigma_mu;
  real<lower=0> k_sex_sigma_mu;
  real<lower=0> s_mu;
}
transformed data {
  // its easier to think in terms of 1/rate for priors
  real<lower=0> l_ind_sigma_rate = 1/l_ind_sigma_mu;
  real<lower=0> l_eth_sigma_rate = 1/l_eth_sigma_mu;
  real<lower=0> l_sex_sigma_rate = 1/l_sex_sigma_mu;
  real<lower=0> k_ind_sigma_rate = 1/k_ind_sigma_mu;
  real<lower=0> k_eth_sigma_rate = 1/k_eth_sigma_mu;
  real<lower=0> k_sex_sigma_rate = 1/k_sex_sigma_mu;
  real<lower=0> s_rate = 1/s_mu;
}
parameters {
  real l_pop;
  real<lower=0> l_sex_sigma;
  vector[2] l_sex;
  real<lower=0> l_eth_sigma;
  vector[4] l_eth;
  real<lower=0> l_ind_sigma;
  vector[N_ind] l_ind;
  real<lower=0> b;
  vector[N_ind] t0;
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
  vector[N_ind] l = l_pop +
    l_sex[sex] * l_sex_sigma +
    l_eth[eth] * l_eth_sigma +
    l_ind * l_ind_sigma;
  vector[N_ind] a = -(l * b);
  vector[N_ind] k = exp(k_pop +
    k_sex[sex] * k_sex_sigma +
    k_eth[eth] * k_eth_sigma +
    k_ind * k_ind_sigma);
  vector[N_ind] m = -(t0 .* k);
}
model {
  l_pop ~ normal(l_pop_mu, l_pop_sigma);
  l_ind_sigma ~ exponential(l_ind_sigma_rate);
  l_ind ~ std_normal();
  l_eth_sigma ~ exponential(l_eth_sigma_rate);
  l_eth ~ std_normal();
  l_sex_sigma ~ exponential(l_sex_sigma_rate);
  l_sex ~ std_normal();
  b ~ lognormal(b_mu, b_sigma);
  k_pop ~ normal(k_pop_mu, k_pop_sigma);
  k_ind_sigma ~ exponential(k_ind_sigma_rate);
  k_ind ~ std_normal();
  k_eth_sigma ~ exponential(k_eth_sigma_rate);
  k_eth ~ std_normal();
  k_sex_sigma ~ exponential(k_sex_sigma_rate);
  k_sex ~ std_normal();
  s ~ exponential(s_rate);
  t0 ~ logistic(l, 1/b);
  for (i in 1:N_obs) {
    real theta = a[pid[i]] + b * age_su[i];
    if (y[i] == 0) {
      target += log1m(inv_logit(theta));
    } else {
      real mu = m[pid[i]] + k[pid[i]] * age_su[i];
      target += log(inv_logit(theta)) + lognormal_lpdf(y[i] | mu, s);
    }
  }
}
generated quantities {
  vector[N_ind] d = log(2) ./ k;
  real hurdle_vll = 0;
  real lognormal_vll = 0;
  real hurdle_lognormal_vll = 0;
  if (N_obs_v > 0) {
    for (i in 1:N_obs_v) {
      real theta_pred = a[pid_v[i]] + b * age_su_v[i];
      if (y_v[i] == 0) {
        hurdle_vll += log1m(inv_logit(theta_pred));
        hurdle_lognormal_vll += log1m(inv_logit(theta_pred));
      } else {
        hurdle_vll += log(inv_logit(theta_pred));
        real mu_pred = m[pid_v[i]] + k[pid_v[i]] * age_su_v[i];
        lognormal_vll += lognormal_lpdf(y_v[i] | mu_pred, s);
        hurdle_lognormal_vll += log(inv_logit(theta_pred)) + lognormal_lpdf(y_v[i] | mu_pred, s);
      }
    }
  }
}
