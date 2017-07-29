

# This function takes a state and 'outcome' (that identifies the disease under concern) 
# and returns the 'best' hospital (i.e., the one with the lowest mortality rate)
# for the given state and outcome

best <- function(state, outcome)
{
  allData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  class(allData)
  
  # key-value pairs of the column (in allData) and their corresponding disease names 
  diseaseNames = c("11"="heart attack", "17"="heart failure", "23"="pneumonia")
  
  #Let's get the list of all states - it's a factor actually so reduce it to factor for faster comparisons later.
  allStates = as.factor(allData[, 7])
  
  #Check if we have been given invalid states or outcomes 
  if(!(state %in% allStates))
    stop("invalid state")
  
  if(!(outcome %in% diseaseNames))
    stop("invalid outcome")
  
  #Let's find out the corresponding mortality rate column index in allData for the given outcome
  disease_col_index <- as.numeric(names(diseaseNames)[which(diseaseNames==outcome)])
  
  #I'll be using the dplry library for data processing here
  library(dplyr)
  
  #Let's select the three columns (hospital, state, mortality rate) we are interested in
  df   <- allData %>% select(c(2, 7, disease_col_index))
  # assign them names for our convenience
  names(df) <- c("Hospital", "State", "Avg.MR")
  # make sure the mortality rates are number so they are not sorted as 'characters'
  df[, "Avg.MR"] <- as.numeric(df[, "Avg.MR"])
  
  #Let's filter to reduce df such that 
  df  <- df %>% filter(State==state) %>%  # filter out those observations with states other than the given state
            arrange(Avg.MR, Hospital) %>% # it is sorted by average mortality rate and THEN by hospital names
            filter(!is.na(Avg.MR)) # filter out all observations with NAs
  
  # The first element has the best hospital
  if(nrow(df) > 0)
    return(df[1,"Hospital"])
  else
    return ("No best Hospital found for the given state and outcome")
}

# Just a function to print out results of the given examples in the assingment - so we can be sure that function is working
testit= function(){
  results = list(c(best("TX", "heart attack"), best("TX", "heart failure"), best("MD", "heart attack"), best("MD", "pneumonia")))
  print(unlist(results))
}
               