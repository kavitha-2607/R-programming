complete <- function(route, id = 1:332){
    directory <- "E:\\R Programming\\rprog_data_specdata"
    ot <- character()
    mat <- matrix(ncol = 2, nrow = length(id)) 
    df <- data.frame(mat)
    colnames(df) <- c("id", "nobs")
    df[1] <- c(id)
    for (i in id){
            xt <- formatC(i, width = 3,flag = 0)
            full <- paste(xt, "csv", sep = ".")
            file <- paste(directory, route, full, sep = "\\")
            data <- read.csv(file)
            nb <- sum(complete.cases(data))
            ot <- c(ot, nb)
         } 
    df[2] <- c(ot)
    print(df)
}

corr <- function(route, threshold = 0){
    directory <- "E:\\R Programming\\rprog_data_specdata"
    id <- 1:332
    ot <- character()
    co <- numeric()
    for (i in id){
            xt <- formatC(i, width = 3,flag = 0)
            full <- paste(xt, "csv", sep = ".")
            file <- paste(directory, route, full, sep = "\\")
            data <- read.csv(file)
            nb <- sum(complete.cases(data))
            if (nb > threshold){
                    calcu <- cor(data$sulfate, data$nitrate, method = "pearson", use = "complete.obs")
                    co <- c(co, calcu)
            }
         }
co    
}