//
// Reinforcement learning (Rescorla-Wagner)
// The participant have been exposed for two different trials
//

data {  // add condition
  int<lower=1> trials;
  array[trials, 2] int<lower=0, upper=1> cond;  //binary: 0=cond 1, 1=cond 2
  array[trials, 2] int<lower=1, upper=2> choice;
  array[trials, 2] int<lower=-1, upper=1> feedback;
}

transformed data {
  vector[2] initValues;  // initial value
  initValues = rep_vector(0.0, 2);
}

parameters {
  // real<lower=0, upper=1> alpha;
  // real<lower=0, upper=1> alpha1;
  // real<lower=0, upper=1> alpha2;
  real alpha1;
  real alpha2;
  real<lower=0> temperature;
}

// transformed parameters{
//   real<lower=0, upper=1> alpha;
//   
//   alpha = alpha1*(1-cond) + alpha2*cond;
// }

model {
  real pe;
  real alpha;
  vector[2] values;
  vector[2] theta;
  
  // priors -> change these from uniform
  // target += uniform_lpdf(alpha1 | 0, 1);
  // target += uniform_lpdf(alpha2 | 0, 1);
  // target += uniform_lpdf(temperature | 0, 20);
  // normally distributed priors
  target += normal_lpdf(alpha1 | 0, .5);
  target += normal_lpdf(alpha2 | 0, .5);
  target += normal_lpdf(temperature | 0, 1);
  
  
  for (c in 1:2){  // loop over the two conditions
  
    values = initValues; 
    
    for (t in 1:trials){  // loop over each trial
    
      theta = softmax(inv_logit(temperature) * values);
      target += categorical_lpmf(choice[t, c] | theta);
      
      alpha = inv_logit(alpha1*(1-cond[t, c]) + alpha2*cond[t, c]);
      pe = feedback[t, c] - values[choice[t, c]];
      values[choice[t, c]] = values[choice[t, c]] + alpha*pe;  //only update value for the chosen choice
    
    }
  }

}

generated quantities {
  real<lower=0, upper=1> alpha1_prior;
  real<lower=0, upper=1> alpha2_prior;
  real<lower=0> temperature_prior;
  real<lower=0, upper=1> alpha1_transformed;
  real<lower=0, upper=1> alpha2_transformed;

  real pe;
  real alpha;
  vector[2] values;
  vector[2] theta;

  real log_lik;

  // priors -> if we change them, we have to change them here too
  // alpha1_prior = uniform_rng(0,1);
  // alpha2_prior = uniform_rng(0,1);
  // temperature_prior = uniform_rng(0,20);
  // normally distributed priors
  alpha1_prior = inv_logit(normal_rng(0, .5));
  alpha2_prior = inv_logit(normal_rng(0, .5));
  temperature_prior = inv_logit(normal_rng(0, 1));
  
  alpha1_transformed = inv_logit(alpha1);
  alpha2_transformed = inv_logit(alpha2);
  

  log_lik = 0;

  for (c in 1:2){  // loop over the two conditions
  
    values = initValues; 
    
    for (t in 1:trials){  // loop over each trial
    
      theta = softmax(temperature * values);
      log_lik = log_lik + categorical_lpmf(choice[t, c] | theta);
      
      alpha = alpha1*(1-cond[t, c]) + alpha2*cond[t, c];
      pe = feedback[t, c] - values[choice[t, c]];
      values[choice[t, c]] = values[choice[t, c]] + alpha*pe;  
    
    }
  }
}