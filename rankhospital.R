rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  library(stringr)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  lst_ot <- list("Heart Attack", "Pneumonia", "Heart Failure")
  outcome <- str_to_title(outcome)
  lst_st <- unique(data[['State']])
  ## Check that state and outcome are valid
  if (!(state %in% lst_st)){
    return ("Invalid state")
  }
  if (!(outcome %in% lst_ot)) {
    return ("Invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  filter <- function(){
    u <- data['State'] == state
    filter_df <<- subset(data, u)
  }
  filter()
  re_col <- function(){
    name_spl<- strsplit(outcome, split = " ")
    if (length(name_spl[[1]])>1){
      outcome <- paste(name_spl[[1]][1], name_spl[[1]][2], sep = ".")
    }
    col <- "Hospital.30.Day.Death..Mortality..Rates.from"
    col_name <<- paste(col, outcome, sep = '.')
    filter_df[col_name] <<- as.numeric(unlist(filter_df[col_name]))
  }
  re_col()

  ## 30-day death rate
  calculateRank <- function(){
    if (num == "best"){
      result <- filter_df[order(filter_df[col_name], filter_df$Hospital.Name), "Hospital.Name"]
      result <<- result[1]
    }
    if (num == "worst") {
      result <<- filter_df[order(filter_df[col_name], filter_df$Hospital.Name, decreasing = TRUE),"Hospital.Name"]
      result <<- result[1]
    }
    if (is.numeric(num)){
      if (num > sum(complete.cases(filter_df['Hospital.Name']))){
        result <<- "NA"
      }
      else{
        result <- filter_df[order(filter_df[col_name], filter_df$Hospital.Name), ]
        result <<- result$Hospital.Name[num]
      }
    }
  }
  calculateRank()
  result
}
