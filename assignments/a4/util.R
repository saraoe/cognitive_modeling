## Util functions

softmax <- function(x, tau){
  1 / (1 + exp(-tau * x))
}


valueUpdate <- function(value, alpha, choice, feedback){
  # value is a list of two values - one for choice 1 and one for choice 2
  # alpha is learning rate
  # choice is 0 if option 1 was chosen, and 1 if option 2 was chosen
  # feedback is either 1 or -1 (expected outcome or unexpected outcome)
  
  v1 <- value[1] + alpha * (1 - choice) * (feedback - value[1])
  v2 <- value[2] + alpha * (choice) * (feedback - value[2])
  
  updatedValues <- c(v1, v2)
  
  return(updatedValues)
}

createSessions <- function(len, trials_in_sessions){
  # takes a length and number of trials in each session
  # returns a list with number for each session
  # that can then be added as a column to the dataframe
  
  session_list <- c()
  
  n_sessions <- len/trials_in_sessions
  for (session in 1:n_sessions){
    session_list <- c(session_list, rep(session, trials_in_sessions))
  }
  
  return(session_list)
}