//
// memory agent modelled as multilevel with partial pooling
//
functions{
  real normal_lb_rng(real mu, real sigma, real lb) {
    real p = normal_cdf(lb | mu, sigma);  // cdf for bounds
    real u = uniform_rng(p, 1);
    return (sigma * inv_Phi(u)) + mu;  // inverse cdf for value
  }
}

// The input (data) for the model. 
data {
 int<lower = 1> trials;
 int<lower = 1> agents;
 array[trials, agents] int h;
 array[trials, agents] real prevRate;
 
 // test data set for crossvalidation
 int<lower = 1> agents_test;
 array[trials, agents_test] int h_test;
 array[trials, agents_test] real prevRate_test;

}

// The parameters accepted by the model. 
parameters {
  real alphaM;
  real<lower = 0> alphaSD;
  real betaM;
  real<lower = 0> betaSD;
  vector[agents] alphaID_z;
  vector[agents] betaID_z;
}

transformed parameters {
  vector[agents] alphaID;
  vector[agents] betaID;
  alphaID = alphaID_z * alphaSD;
  betaID = betaID_z * betaSD;
 }

// The model to be estimated. 
model {
  target += normal_lpdf(alphaM | 0, 1);
  target += normal_lpdf(alphaSD | 0, .3)  -
    normal_lccdf(0 | 0, .3);
  target += normal_lpdf(betaM | 0, .3);
  target += normal_lpdf(betaSD | 0, .3)  -
    normal_lccdf(0 | 0, .3);

  target += std_normal_lpdf(to_vector(alphaID_z));
  target += std_normal_lpdf(to_vector(betaID_z));
 
  for (i in 1:agents){
    target += bernoulli_logit_lpmf(h[,i] | alphaM + alphaID[i] +  to_vector(prevRate[,i]) * (betaM + betaID[i]));
  }
  
  
  
}

generated quantities{
   real alphaM_prior;
   real<lower=0> alphaSD_prior;
   real betaM_prior;
   real<lower=0> betaSD_prior;
   
   real alpha_prior;
   real beta_prior;
   real alpha_test;
   real beta_test;
   
   array[trials,agents] int<lower=0, upper = trials> prior_preds;
   array[trials,agents] int<lower=0, upper = trials> posterior_preds;
   
   array[trials, agents] real log_lik;
   array[trials, agents_test] real log_lik_test;
   
   
   alphaM_prior = normal_rng(0,1);
   alphaSD_prior = normal_lb_rng(0,0.3,0);
   betaM_prior = normal_rng(0,1);
   betaSD_prior = normal_lb_rng(0,0.3,0);
   
   alpha_prior = normal_rng(alphaM_prior, alphaSD_prior);
   beta_prior = normal_rng(betaM_prior, betaSD_prior);
   
   alpha_test = normal_rng(alphaM, alphaSD);
   beta_test = normal_rng(betaM, betaSD);
   
   for (i in 1:agents){
      prior_preds[,i] = binomial_rng(trials, inv_logit(alpha_prior + to_vector(prevRate[,i]) * beta_prior));
      posterior_preds[,i] = binomial_rng(trials, inv_logit(alphaM + alphaID[i] +  to_vector(prevRate[,i]) * (betaM + betaID[i])));
      
    for (t in 1:trials){
      log_lik[t,i] = bernoulli_logit_lpmf(h[t,i] | alphaM + alphaID[i] +  prevRate[t,i] * (betaM + betaID[i]));
    }
  }
  
  for (i in 1:agents_test){

    for (t in 1:trials){
      log_lik_test[t,i] = bernoulli_logit_lpmf(h_test[t,i] | alpha_test +  prevRate_test[t,i] * (beta_test));
    }
  }
   
}


