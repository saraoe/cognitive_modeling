//
// This STAN model infers a random bias from a sequences of 1s and 0s (heads and tails). Now multilevel
//

functions{
  real normal_lb_rng(real mu, real sigma, real lb) {
    real p = normal_cdf(lb | mu, sigma);  // cdf for bounds
    real u = uniform_rng(p, 1);
    return (sigma * inv_Phi(u)) + mu;  // inverse cdf for value
  }
}

// The input (data) for the model. n of trials and h of heads
data {
 int<lower = 1> trials;
 int<lower = 1> agents;
 array[trials, agents] int h;
 
 // test data set for crossvalidation
 int<lower = 1> agents_test;
 array[trials, agents_test] int h_test;
}

// The parameters accepted by the model. 
parameters {
  real thetaM;
  real<lower = 0> thetaSD;
  vector[agents] thetaID_z;
}

transformed parameters {
  vector[agents] thetaID;
  thetaID = thetaID_z * thetaSD;
 }

// The model to be estimated. 
model {
  target += normal_lpdf(thetaM | 0, 1);
  target += normal_lpdf(thetaSD | 0, .3)  -
    normal_lccdf(0 | 0, .3);

  // The prior for theta is a uniform distribution between 0 and 1
  //target += normal_lpdf(theta | thetaM, thetaSD); 
  target += std_normal_lpdf(to_vector(thetaID_z));
 
  for (i in 1:agents)
    target += bernoulli_logit_lpmf(h[,i] | thetaM+thetaID_z[i]);
  
}


generated quantities{
   real thetaM_prior;
   real<lower=0> thetaSD_prior;
   real<lower=0, upper=1> theta_prior;
   real<lower=0, upper=1> theta_posterior;
   
   array[trials,agents] int<lower=0, upper = trials> prior_preds;
   array[trials,agents] int<lower=0, upper = trials> posterior_preds;
   
   array[trials, agents] real log_lik;
   array[trials, agents_test] real log_lik_test;

   thetaM_prior = normal_rng(0,1);
   thetaSD_prior = normal_lb_rng(0,0.3,0);
   theta_prior = inv_logit(normal_rng(thetaM_prior, thetaSD_prior));
   theta_posterior = inv_logit(normal_rng(thetaM, thetaSD));
   
   for (i in 1:agents){
     
    for (t in 1:trials){
      prior_preds[t,i] = binomial_rng(trials, inv_logit(theta_prior));
      posterior_preds[t,i] = binomial_rng(trials, inv_logit(thetaM+thetaID_z[i]));
      log_lik[t,i] = bernoulli_logit_lpmf(h[t,i] | thetaM+thetaID_z[i]);
      
    }
  }
  for (i in 1:agents_test){
    
    for (t in 1:trials){
      log_lik_test[t,i] = bernoulli_logit_lpmf(h_test[t,i] | theta_posterior);
      
    }
  }
}

