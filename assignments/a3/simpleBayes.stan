//
// simple bayes model
//


data {
  int<lower = 1> trials;
  int<lower = 1> participants;
  array[trials, participants] real y;
  array[trials, participants] real Source1;
  array[trials, participants] real Source2;
}

parameters {
  real<lower = 0> sigma;
}

model {
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

