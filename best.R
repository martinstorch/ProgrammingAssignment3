best <- function(state, outcome) {
  columns <- list(
    "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia"= "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    )
  
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  for (colname in columns) {
    outcome_data[,colname]<-as.numeric(outcome_data[,colname])
  }

  #print(str(outcome_data))
  
  ## Check that state and outcome are valid
  if (all(outcome != names(columns))) 
      stop("invalid outcome")
      
  if (all(state != outcome_data$State)) 
      stop("invalid state")
          
  ## Return hospital name in that state with lowest 30-day death
  
  state_outcome <- outcome_data[outcome_data$State==state , c("State", "Hospital.Name", columns[[outcome]])]
   ## rate
  sorted_state_outcomes <- state_outcome[order(state_outcome[,3], state_outcome[,2]),]
  sorted_state_outcomes[1,"Hospital.Name"]
}
