// memory agent model

// n is number of trials, h is the choice, r is the previous rate
data {
  int<lower=1> n;
  array[n] int h;
  vector<lower=0, upper=1>[n] r;
  real prior_mean_alpha;
  real prior_mean_beta;
  real<lower=0> prior_sd_alpha;
  real<lower=0> prior_sd_beta;
}

parameters {
  real alpha;
  real beta;
}

model {
  // priors are normally distributed
  target += normal_lpdf(alpha | prior_mean_alpha, prior_sd_alpha);
  target += normal_lpdf(beta | prior_mean_beta, prior_sd_beta);
  
  // the model is a binomial distribution with rate theta
  target += bernoulli_logit_lpmf(h | alpha + beta*r);
}

generated quantities{
  real alpha_prior;
  real beta_prior;
  array[n] int prior_preds;
  array[n] int posterior_preds;


  alpha_prior = normal_rng(prior_mean_alpha, prior_sd_alpha);
  beta_prior = normal_rng(prior_mean_beta, prior_sd_beta);
  prior_preds = binomial_rng(n, inv_logit(alpha_prior+beta_prior*r));
  posterior_preds = binomial_rng(n, inv_logit(alpha+beta*r));
}



