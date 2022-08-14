rankall <- function(outcome, num = "best") {
  ## Read outcome data
  library(stringr)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  lst_ot <- list("Heart Attack", "Pneumonia", "Heart Failure")
  outcome <- str_to_title(outcome)
  if (!(outcome %in% lst_ot)) {
    return ("Invalid outcome")
  }
  re_col <- function(){
    name_spl<- strsplit(outcome, split = " ")
    if (length(name_spl[[1]])>1){
      outcome <- paste(name_spl[[1]][1], name_spl[[1]][2], sep = ".")
    }
    col <- "Hospital.30.Day.Death..Mortality..Rates.from"
    col_name <<- paste(col, outcome, sep = '.')
  }
  re_col()
  lst_states = unique(data[['State']])
  h <- character()
  st <- character()

  for (state in lst_states){
    filter <- function(state){
      u <- data['State'] == state
      filter_df <<- subset(data, u)
      filter_df[col_name] <<- as.numeric(unlist(filter_df[col_name]))
    }
    filter(state)
    calculateRank <- function(){
      if (num == "best"){
        result <- filter_df[order(filter_df[col_name], filter_df$Hospital.Name), ]
        h <<- c(h, result$Hospital.Name[1])
        st <<- c(st, result$State[1])
      }
      if (num == "worst") {
        result <- filter_df[order(filter_df[col_name], filter_df$Hospital.Name, decreasing = TRUE), ]
        h <<- c(h, result$Hospital.Name[1])
        st <<- c(st, result$State[1])
      }
      if (is.numeric(num)){
        if (num > sum(complete.cases(filter_df['Hospital.Name']))){
          h <<- c(h, NA)
          st <<- c(st, state)
        }
        else{
          result <- filter_df[order(filter_df[col_name], filter_df$Hospital.Name), ]
          h <<- c(h, result$Hospital.Name[num])
          st <<- c(st, result$State[num])
        }
      }
    }
    calculateRank()
  }
  mat <- matrix(nrow = length(h), ncol = 2)
  columns <- c("hospital", "state")
  results_df <- data.frame(mat)
  colnames(results_df) <- columns
  results_df["hospital"] <- h
  results_df["state"] <- st
  print(results_df[order(results_df['state']), ])
}
