####################################################
## Strategies for agents in matching pennies game ##
####################################################

# random agent with bias and noise
RandomAgent <- function(bias, noise){  
    choice <- rbinom(1, 1, bias)
    
    if(rbinom(1, 1, noise)==1){  # noise
      choice <- rbinom(1,1,0.5)
      }
    
    return(choice)
}

# win-stay-lose-shift
WSLSAgent <- function(prevChoice, feedback, noise){ 
  if(feedback == 1){  # win
    choice <- prevChoice
  } else if (feedback == 0) {  # lose
      choice <- 1 - prevChoice 
  }
  
  if(rbinom(1, 1, noise)==1){  # noise
    choice <- rbinom(1,1,0.5)
  }
  
  return(choice)
}

# WSLS with probability
WSLSAgent_prob <- function(prevChoice, Feedback, Prob, noise){
  if (Feedback == 1){
    choice = sample(c(prevChoice, 1 - prevChoice), size = 1, replace = TRUE, prob = c(Prob, 1 - Prob))
  } else if (Feedback == 0) {
    choice = sample(c(prevChoice, 1 - prevChoice), size = 1, replace = TRUE, prob = c(1- Prob, Prob)) }
  
  if(rbinom(1, 1, noise)==1){  # noise
    choice <- rbinom(1,1,0.5)
  }
  return(choice) }

# memory agent
MemoryAgent <- function(memory, mem_contraint, noise){
  if (mem_contraint < length(memory)){
    index <-length(memory) - mem_contraint
    memory <- memory[index:length(memory)]
  }
  prob1 <- mean(memory)
  
  if (prob1 > 0.5){
    choice <- RandomAgent(1, noise)
  } else if (prob1 < 0.5) {
    choice <- RandomAgent(0, noise)
  } else {
    choice <- RandomAgent(0.5, noise)
  }
  
  return(choice)
}