best <- function(state, outcome){
  library(stringr)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  lst_ot <- list("Heart Attack", "Pneumonia", "Heart Failure")
  outcome <- str_to_title(outcome)
  lst_st <- unique(data[['State']])
  if (!(state %in% lst_st)){
    return ("Invalid state")
  }
  if (!(outcome %in% lst_ot)) {
    return ("Invalid outcome")
  }
  else{
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
    calculateMin <- function(){
      mn <- lapply(filter_df[col_name], min, na.rm = TRUE)
      f <- filter_df[col_name] == mn
      re_filter_df <<- subset(filter_df, f)
      if (length(re_filter_df['Hospital.Name'])>1){
        rank <- re_filter_df['Hospital.Name']
        first_rk <- order(rank)
        result <<- rank[first_rk, ][1]
      }
      else {result <<- re_filter_df[['Hospital.Name']]}
    }
  }
  calculateMin()
  print (result)
}
