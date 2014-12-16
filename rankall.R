rankall <- function(outcome, num="best") {
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

  ## Check that state and outcome are valid
  if (all(outcome != names(columns))) 
      stop("invalid outcome")
      
  if (num < 1 | is.character(num) & all(num != c("best", "worst")))
    stop("invalid rank")
    
  ## Select the required columns from the raw data
    all_data <- outcome_data[, c("State", "Hospital.Name", columns[[outcome]])]
  # eliminate NAs
  all_data <- all_data[complete.cases(all_data),]
  
  # split by state and sort each sublist
  per_state_data <- split(all_data, all_data$State)
  sorted_state_data <- lapply(per_state_data, function (x) x[order(x[,3], x[,2]),])
  
  num <- if (num=="best") 1 else num
  
  # Get the selected rank or "worst" from each list as a list of data frames. 
  # Each data frame in the return list will contain one row only
  # NA_character_ is NA (logical) coerced to character, this is required for vapply() to character(1)
  result <- lapply(sorted_state_data, function(x) {
    if (num == "worst") tail(x, 1) 
    else if (num > nrow(x)) {y<-head(x,1); y$Hospital.Name <- NA_character_; y}
    else x[num,]
  })

  # construct the output data frame from the list of data frames
  rresult <- data.frame(
              hospital=vapply(result, function(x) {x[["Hospital.Name"]]}, character(1)),
              state=names(result))
  
  rresult
}
