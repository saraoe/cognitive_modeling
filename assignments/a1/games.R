#####################################################
## Functions for agents playing against each other ##
#####################################################

## RandomAgent (mismatcher) against memoryAgent (matcher)
RandomAgainstMemory <- function(trials, rate, noise, mem_constraint){
  matcher <- rep(NA, trials)
  mismatcher <- rep(NA, trials)
  
  matcher[1] <- RandomAgent(0.5,0)
  
  # choices of the random agent
  for (t in seq(trials)){
    mismatcher[t] <- RandomAgent(rate, noise)
  }
  
  # choices of memory agent
  memory <- c(mismatcher[1])  # initialize memory
  for (i in 2:trials){
    matcher[i] <- MemoryAgent(memory, mem_constraint, noise)
    memory <- c(mismatcher[i], memory)
  }
  
  df <-tibble(matcher, mismatcher, trial =seq(trials), Feedback =as.numeric(matcher==mismatcher))
  df <- df %>% mutate(
    cumulativeMatcher = cumsum(Feedback)/seq_along(Feedback),
    cumulativeMismatcher = cumsum(1-Feedback)/seq_along(Feedback)
  )
  return(df)
}


## WSLSAgent (mismatcher) against MemoryAgent (matcher)
WSLSAgainstMemory <- function(trials, noise, mem_constraint){
  matcher <- rep(NA, trials)
  mismatcher <- rep(NA, trials)
  
  matcher[1] <- RandomAgent(0.5,0)
  mismatcher[1] <- RandomAgent(0.5,0)
  
  memory <- c(mismatcher[1])  # initialize memory
  
  for (i in 2:trials){
    # choice of memory agent (matcher)
    matcher[i] <- MemoryAgent(memory, mem_constraint, noise)
    # choice of WSLS agent (mismatcher)
    if (mismatcher[i-1]==matcher[i-1]){
      Feedback <- 1
    } else {
      Feedback <- 0
    }
    mismatcher[i] <- WSLSAgent(mismatcher[i-1], 1-Feedback, noise)
    # update memory
    memory <- c(memory, mismatcher[i])
  }
  
  df <-tibble(matcher, mismatcher, trial =seq(trials), Feedback =as.numeric(matcher==mismatcher))
  df <- df %>% mutate(
    cumulativeMatcher = cumsum(Feedback)/seq_along(Feedback),
    cumulativeMismatcher = cumsum(1-Feedback)/seq_along(Feedback)
  )
  return(df)
}

