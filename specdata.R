library(dplyr)
library(readr)
options(readr.show_col_types = FALSE)
route <- "specdata"
pollutantmean <- function(route, pollutant, id = 1:332, removeNa = TRUE){
directory <- "E:\\R Programming\\rprog_data_specdata"
pa <- paste(directory, route, sep = "\\")
filenames <- list.files(path = pa, pattern = ".csv", all.files = TRUE)
filenames <- filenames[id]
dataset <- do.call("rbind",lapply(filenames,FUN=function(files){ read.csv(files)}))
mn <- lapply(dataset[pollutant], mean, na.rm = removeNa)
print(mn)
}