//
// simple bayes model
//


data {
  int<lower = 1> trials;
  int<lower = 1> agents;
  array[trials, agents] int choice;
  array[trials, agents] real Source1;
  array[trials, agents] real Source2;
}

parameters {
  real<lower = 0> sigma;
}

model {
  target += normal_lpdf(sigma | 0, .3)  -
    normal_lccdf(0 | 0, .3);
  
  for (a in 1:agents){
    for (t in 1:trials){
     target += normal_lpdf(choice[t,a] | logit(Source1[t,a]) +  logit(Source2[t,a]), sigma); 
    }
  } 
}

generated quantities{
  array[trials, agents] real log_lik;
  
  for (a in 1:agents){
    for (t in 1:trials){
     log_lik[t,a] = normal_lpdf(choice[t,a] | logit(Source1[t,a]) +  logit(Source2[t,a]), sigma); 
    }
  } 
  
}

