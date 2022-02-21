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
  if(Feedback == 1){  # win
    choice <- prevChoice
  } else if (Feedback == 0) {  # lose
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

MemoryAgent <- function(othersChoice, memory, N, noise){
  memory <- memory+othersChoice
  
  choice <- rbinom(1,1,memory/N)
  
  if(rbinom(1, 1, noise)==1){  # noise
    choice <- rbinom(1,1,0.5)
  }
  
  return(c(choice, memory))
}