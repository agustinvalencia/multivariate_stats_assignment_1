# Q1

data_path <- "data/T1-9.dat"
raw_data <- read.table(data_path)
# removing text data to work easily
data <- raw_data[, !(names(raw_data) == "V1")]

Q1_a <- function () {
    # Means
    column_means <- colMeans(data)
    
    # Variances
    vars <- var(data)
    
    # Correlations
    cors <- cor(data)
    
    # Total sample variance
    total_sample_var <- sum(diag(vars))
    
    # Generalized sample variance
    gen_sample_var <- det(vars)
    
    ## Summary
    cat("** Summarizing data **")
    cat("\n\n*data path: ", data_path)
    
    cat("\n\n*Column means: \n")
    print(column_means)
    
    cat("\n*Variances: \n")
    print(vars)
    
    cat("\n*Correlations: \n")
    print(cors)
    
    cat("\n*Total Sample Variance: \n")
    print(total_sample_var)
    
    cat("\n*Generalized Sample Variance: \n")
    print(gen_sample_var)
}


Q1_b <- function() {
    scatterplotMatrix(data)
}