//
// Reinforcement learning (Rescorla-Wagner)
// The participant have been exposed for two different trials
//

data {
  int<lower=1> trials;
  array[trials, 2] int<lower=0, upper=1> cond;  //binary: 0=cond 1, 1=cond 2
  array[trials, 2] int<lower=1, upper=2> choice;
  array[trials, 2] int<lower=-1, upper=1> feedback;
  // priors
  array[2] real alpha1_prior_values;
  array[2] real alpha2_prior_values;
  array[2] real temp_prior_values;
}

transformed data {
  vector[2] initValues;  // initial value
  initValues = rep_vector(0.0, 2);
}

parameters {
  real alpha1;
  real alpha2;
  real temperature;
}

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
  target += normal_lpdf(alpha1 | alpha1_prior_values[1], alpha1_prior_values[2]);
  target += normal_lpdf(alpha2 | alpha2_prior_values[1], alpha2_prior_values[2]);
  target += normal_lpdf(temperature | temp_prior_values[1], temp_prior_values[2]);
  
  
  for (c in 1:2){  // loop over the two conditions
  
    values = initValues; 
    
    for (t in 1:trials){  // loop over each trial
    
      theta = softmax(inv_logit(temperature)*20 * values);
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
  real<lower=0> temperature_transformed;
  real alpha_diff;

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
  alpha1_prior = inv_logit(normal_rng(alpha1_prior_values[1], alpha1_prior_values[2]));
  alpha2_prior = inv_logit(normal_rng(alpha2_prior_values[1], alpha2_prior_values[2]));
  temperature_prior = inv_logit(normal_rng(temp_prior_values[1], temp_prior_values[2])) * 20;
  
  alpha1_transformed = inv_logit(alpha1);
  alpha2_transformed = inv_logit(alpha2);
  temperature_transformed = inv_logit(temperature)*20;
  
  alpha_diff = alpha2_transformed - alpha1_transformed;
  

  log_lik = 0;

  for (c in 1:2){  // loop over the two conditions
  
    values = initValues; 
    
    for (t in 1:trials){  // loop over each trial
    
      theta = softmax(temperature * values);
      log_lik = log_lik + categorical_lpmf(choice[t, c] | theta);
      
      alpha = inv_logit(alpha1*(1-cond[t, c]) + alpha2*cond[t, c]);
      pe = feedback[t, c] - values[choice[t, c]];
      values[choice[t, c]] = values[choice[t, c]] + alpha*pe;  
    
    }
  }
}