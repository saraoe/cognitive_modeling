// memory agent model

data {
  // Observed variables:
  // n is number of trials
  int<lower=1> n;
  // h is the choice of the memory agent
  array[n] int h;
  // r is the previous rate of the opponent
  vector<lower=0, upper=1>[n] r;
  
  // priors for the parameters alpha and beta:
  real prior_mean_alpha;
  real prior_mean_beta;
  real<lower=0> prior_sd_alpha;
  real<lower=0> prior_sd_beta;
}

// the parameters alpha and beta are the ones we want to estiamate
parameters {
  real alpha;
  real beta;
}

model {
  // priors for alpha and beta are normally distributed
  target += normal_lpdf(alpha | prior_mean_alpha, prior_sd_alpha);
  target += normal_lpdf(beta | prior_mean_beta, prior_sd_beta);
  
  // the model is a binomial distribution with rate alpha + beta*r
  target += bernoulli_logit_lpmf(h | alpha + beta*r);
}

generated quantities{
  // prior distributions for alpha and beta
  real alpha_prior;
  real beta_prior;
  // prior and posterior predictions
  // as the predictions depend on previous rate they are an array of length of trials
  array[n] int prior_preds;
  array[n] int posterior_preds;
  
  // the priors are normally distributed
  alpha_prior = normal_rng(prior_mean_alpha, prior_sd_alpha);
  beta_prior = normal_rng(prior_mean_beta, prior_sd_beta);
  // the agents rate of choosing head or tails is the inv_logit of the rate (alpha+beta*r)
  // prior predictions use the prior alpha and beta estimates
  prior_preds = binomial_rng(n, inv_logit(alpha_prior+beta_prior*r));
  // posterior predictions use the posterior alpha and beta estimates
  posterior_preds = binomial_rng(n, inv_logit(alpha+beta*r));
}



