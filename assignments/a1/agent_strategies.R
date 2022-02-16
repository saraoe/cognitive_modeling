####################################################
## Strategies for agents in matching pennies game ##
####################################################

# random agent with bias and noise
RandomAgent <- function(bias, noise){  
    choice <- rbinom(1, 1, bias)
    
    if(rbinom(1, 1, noise)==1){  # noise
      choice = rbinom(1,1,0.5)
      }
    
    return(choice)
}

# win-stay-lose-shift
WSLSAgent <- function(prevChoice, feedback, noise){ 
  if(Feedback == 1){  # win
    choice = prevChoice
  } elif (Feedback == 0) {  # lose
      choice = 1 - prevChoice 
  }
  
  if(rbinom(1, 1, noise)==1){  # noise
    choice = rbinom(1,1,0.5)
  }
  
  return(choice)
}