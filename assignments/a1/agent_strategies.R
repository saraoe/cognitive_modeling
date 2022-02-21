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

# keep track of bias
# MemoryAgent <- function(othersChoice, learningRate, noise){
#   if(othersChoice==1){
#     memory = (1-learningRate)*memory + learningRate*1
#   } else if (othersChoice==0){
#     memory = (1-learningRate)*memory + learningRate*0
#   }
#   
#   choice = rbinom(1,1,memory)
#   
#   if(rbinom(1, 1, noise)==1){  # noise
#     choice = rbinom(1,1,0.5)
#   }
#   
#   return(choice)
# }

# MemoryAgent <- function(othersChoice, memory, N, noise){
#   memory <- memory+othersChoice
#   
#   choice <- rbinom(1,1,memory/N)
#   
#   if(rbinom(1, 1, noise)==1){  # noise
#     choice <- rbinom(1,1,0.5)
#   }
#   
#   return(c(choice, memory))

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