//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//


data {
  int<lower = 1> trials;
  int<lower = 1> participants;
  array[trials, participants] real y;
  array[trials, participants] real Source1;
  array[trials, participants] real Source2;
}

parameters {
  // real sigmaM;
  // real<lower = 0> sigmaSD;
  // array[participants] real sigma;
  real<lower = 0> sigma;
}

model {
  // target += normal_lpdf(sigmaM | 0, 1);
  // target += normal_lpdf(sigmaSD | 0, .3)  -
  //   normal_lccdf(0 | 0, .3);
  // 
  // target += normal_lpdf(sigma | sigmaM, sigmaSD)  -
  //   normal_lccdf(0 | sigmaM, sigmaSD);
  
  target += normal_lpdf(sigma | 0, .3)  -
    normal_lccdf(0 | 0, .3);
  
  for (p in 1:participants){
    for (t in 1:trials){
     target += normal_lpdf(y[t,p] | logit(Source1[t,p]) +  logit(Source2[t,p]), sigma); 
    }
  } 
}

generated quantities{
  array[trials, participants] real log_lik;
  
  for (p in 1:participants){
    for (t in 1:trials){
     log_lik[t,p] = normal_lpdf(y[t,p] | logit(Source1[t,p]) +  logit(Source2[t,p]), sigma); 
    }
  } 
  
}

