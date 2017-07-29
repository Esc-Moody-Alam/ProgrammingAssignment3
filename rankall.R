
# This function takes a state, 'outcome' (that identifies the disease under concern) and a ranking number
# and returns the 'best' hospital (i.e., the one with the lowest mortality rate)
# for the given state and outcome


rankall <- function(outcome, num ="best")
{

  allData <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors=FALSE)
  
  # key-value pairs of the column (in allData) and their corresponding disease names 
  diseaseNames <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)

  #Let's get the list of all states - it's a factor actually so reduce it to factor for faster comparisons later.
  #allStates = as.factor(allData[, 7])
  
  #Check if we have been given invalid outcomes
  if(!(outcome %in% names(diseaseNames)))
    stop("invalid outcome")
  
  #Let's find out the corresponding mortality rate column index in allData for the given outcome
  disease_col_index <- as.numeric(diseaseNames[which(names(diseaseNames)==outcome)])
  #I'll be using the dplry library for data processing here
  library(dplyr)
  
  #Let's select the three columns (hospital, state, mortality rate) we are interested in
  df   <- allData[, c(2, 7, disease_col_index)]
  
  # assign them names for our convenience
  names(df) <- c("Hospital", "State", "MortalityRate")

  num_eval <- function(x)
  {
    y <- x
    x <- x[!is.na(x[,"MortalityRate"]),]
    x <- x[order(x[, 3]),]
    
    if(nrow(x)==0) return(c(NA, y$State[1], NA))
    
    if(num=="best") return(head(x,1))
    if(num=="worst") return(tail(x,1))
    if(num > nrow(x)) return(c(NA, y$State[1], NA))
    
    x <- c(x[num,1], x[num,2], x[num,3])
  }
  
  df <- lapply(split(df, df$State), num_eval)

  df <- data.frame(matrix(unlist(df), nrow=length(df), byrow=T))
  names(df) <- c("Hospital", "State", "MortalityRate")
  df

}

# Just a function to print out results of the given examples in the assingment - so we can be sure that function is working
print (head(rankall("heart attack", 20), 10))
print(tail(rankall("pneumonia", "worst"), 3))
print(tail(rankall("heart failure"), 10))

               