// stan model: random agent

data {
  int<lower=1> n;
  array[n] int h;
}

parameters {
  real theta;
}

model {
  target += normal_lpdf(theta | 0, 1);
  
  target += bernoulli_logit_lpmf(h | theta);
}

generated quantities{
  real<lower=0, upper=1> theta_p;
  theta_p = inv_logit(theta);
}

