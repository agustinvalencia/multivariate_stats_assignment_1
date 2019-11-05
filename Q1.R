# Q1

Q1 <- function () {
    data_path <- "data/T1-9.dat"
    raw_data <- read.table(data_path)
    
    # removing text data to work easily
    data <- raw_data[, !(names(raw_data) == "V1")]
    
    # Means
    column_means <- colMeans(data)
    
    # Variances
    vars <- var(data)
    
    # Correlations
    cors <- cor(data)
    
    ## Summary
    cat("** Summarizing data **")
    cat("\n\n*data path: ", data_path)
    
    cat("\n\n*Column means: \n")
    print(column_means)
    
    cat("\n*Variances: \n")
    print(vars)
    
    cat("\n*Correlations: \n")
    print(cors)
}