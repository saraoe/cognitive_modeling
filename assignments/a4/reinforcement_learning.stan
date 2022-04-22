//
// Reinforcement learning (Rescorla-Wagner)
// The participant have been exposed for two different trials
//

data {  // add condition
  int<lower=1> trials;
  array[trials] int<lower=1, upper=2> choice;
  array[trials] int<lower=-1, upper=1> feedback;
}

transformed data {
  vector[2] initValues;  // initial value
  initValues = rep_vector(0.0, 2);
}

parameters {
  // real<lower=0, upper=1> alpha;
  real alpha1;
  real alpha2;
  real<lower=0> temperature;
}

transformed parameters{
  real<lower=0, upper=1> alpha;
  
  alpha = alpha1*(cond-1) + alpha2*cond;
}

model {
  real pe;
  vector[2] values;
  vector[2] theta;
  
  // priors -> change these from uniform
  target += uniform_lpdf(alpha | 0, 1);
  target += uniform_lpdf(temperature | 0, 20);
  
  values = initValues;
  
  for (t in 1:trials){
    theta = softmax(temperature * values);
    target += categorical_lpmf(choice[t] | theta);
    
    pe = feedback[t] - values[choice[t]];
    values[choice[t]] = values[choice[t]] + alpha*pe;  //only update value for the chosen choice 
    
  }
}

generated quantities {
  real<lower=0, upper=1> alpha_prior;
  real<lower=0, upper=10> temperature_prior;

  real pe;
  vector[2] values;
  vector[2] theta;

  real log_lik;

  // priors -> if we change them, we have to change them here too
  alpha_prior = uniform_rng(0,1);
  temperature_prior = uniform_rng(0,20);

  values = initValues;
  log_lik = 0

  for (t in 1:trials){
    theta = softmax(temperature * values);
    log_lik = log_lik + categorical_lpmf(choice[t] | theta);

    pe = feedback[t] - values[choice[t]];
    values[choice[t]] = values[choice[t]] + alpha*pe;
  }
}