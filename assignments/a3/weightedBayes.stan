//
// Weighted Bayes modelled with multilevel
//

functions{
  real weight_f(real L_raw, real w_raw) {
    real L;
    real w;
    L = exp(L_raw);
    w = 0.5 + inv_logit(w_raw)/2;
    return log((w * L + 1 - w)./((1 - w) * L + w));
  }
}

data {
  int<lower = 1> trials;
  int<lower = 1> agents;
  array[trials, agents] int choice;
  array[trials, agents] real Source1;
  array[trials, agents] real Source2;
}

parameters {
  // Mean of the distributions of weights for source 1 and 2 in the overall distributions.
  real weight1M;
  real weight2M; 
  // Since the choice is modeled as a sample from a normal distribution with mean = b1*source1 +b2*source2, we also need a sigma for the distribution. 
  real<lower=0> sigma;
  
  // tau is a vector of length = 2, with the sigmas for overall weight 1 and overall weight 2
  vector<lower=0>[2] tau;
  // z_IDs is a matrix that contains - for each participant - the deviation from the overall mean of weight 1, then z-scored (divided by tau). 
  // The z-scoring ensures that Z_ID's are centered around 1.
  matrix[2, agents] z_IDs; 
  // A correlation coefficient that accounts for the fact that when we're sampling a high mean of weight 1 for an individual participant, 
  // that we are more likely to sample a low value for weight 2.
  cholesky_factor_corr[2] L_u; 
}

transformed parameters {
  matrix[agents,2] IDs; 
  IDs = (diag_pre_multiply(tau, L_u) * z_IDs);
}

model {
  // priors
  target += normal_lpdf(weight1M | 0, 1);
  target += normal_lpdf(weight2M | 0, 1);
  target += normal_lpdf(tau[1] | 0, .3) - normal_lccdf(0 | 0, .3);
  target += normal_lpdf(tau[2] | 0, .3) - normal_lccdf(0 | 0, .3);
  target += lkj_corr_cholesky_lpdf(L_u | 2);
  
  target += std_normal_lpdf(to_vector(z_IDs));
  
  // model
  for (agent in 1:agents){
    for (trial in 1:trials){
      target += normal_lpdf(choice[trial, agent] |
        weight_f(Source1[trial, agent], weight1M + IDs[1,agent]) +
        weight_f(Source2[trial, agent], weight2M + IDs[2, agent]),
        sigma);
    }
  }
}

generated quantities{
  array[trials, agents] real log_lik;
  real w1M_prior;
  real w2M_prior;
  real w1M;
  real w2M;
  
  w1M_prior = 0.5 + inv_logit(normal_rng(0,1))/2;
  w2M_prior = 0.5 + inv_logit(normal_rng(0,1))/2;

  w1M = 0.5 + inv_logit(weight1M)/2;
  w2M = 0.5 + inv_logit(weight2M)/2;
  
  for (agent in 1:agents){
    for (trial in 1:trials){
      log_lik[trial, agent] = normal_lpdf(choice[trial, agent] |
        weight_f(Source1[trial, agent], weight1M + IDs[1, agent]) +
        weight_f(Source2[trial, agent], weight2M + IDs[2, agent]),
        sigma);
    }
  }
  
}
