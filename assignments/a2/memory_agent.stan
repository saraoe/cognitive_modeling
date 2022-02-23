// memory agent model

// n is number of trials, h is the choice, r is the previous rate
data {
  int<lower=1> n;
  array[n] int h;
  vector[n] r;
}

parameters {
  real alpha;
  real beta;
}

model {
  target += normal_lpdf(alpha | 0, 1);
  target += normal_lpdf(beta | 0, 0.5);
  
  target += bernoulli_logit_lpmf(h | alpha + beta*r);
}


