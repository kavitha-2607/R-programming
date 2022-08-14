complete <- function(route, id = 1:332){
ot <- character()
mat <- matrix(ncol = 2, nrow = length(id)) 
df <- data.frame(mat)
colnames(df) <- c("id", "nobs")
df[1] <- c(id)
directory <- "E:\\R Programming\\rprog_data_specdata"
pa <- paste(directory, route, sep = "\\")
filenames <- list.files(path = pa, pattern = ".csv", all.files = TRUE)
filenames <- filenames[id]
for (file in filenames){
            data <- read.csv(file)
            nb <- sum(complete.cases(data))
            ot <- c(ot, nb)
         } 
df[2] <- c(ot)
print(df)
}