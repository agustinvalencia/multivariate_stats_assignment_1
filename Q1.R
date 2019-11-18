library(car)
library(reshape)
library(ggplot2)
# Q1

data_path <- "data/T1-9.dat"
data <- read.table(data_path)
#convert seconds to minutes in the first three cateogories
data[,2:4] <- data[,2:4]/60

Q1_a <- function () {
    data <- data[,2:8]
    
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
    
    #Boxplot per race category (Marcos)
    #assigning names to columns for easier reading
    colnames(data) <- c("country", "100m", "200m", "400m", "800m", "1500m", "3000m", "marathon")
    
    scatterplotMatrix(data[,2:8])
    
    #melted data
    meltdata <- melt(data)
    
    #a)Data description with mean and standard deviation
    #mean
    means <- sapply(data[,2:8], mean)
    #standard deviations
    sds <- sapply(data[,2:8], sd)
    
    #b) Illustrate variables with different graphs
    p <- ggplot(data = meltdata, aes(x = variable, y = value )) +
        geom_boxplot(aes(fill = variable)) +
        labs(x = "category", y = "Track records (in minutes)")
    plot <- p + facet_wrap(~variable, scales = "free")
    cat("\n")
    plot
}