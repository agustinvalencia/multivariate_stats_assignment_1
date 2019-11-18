library(car)
library(reshape)
library(ggplot2)
# Q1

data_path <- "data/T1-9.dat"
raw_data <- read.table(data_path)
# removing text data to work easily
data <- raw_data

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



    scatterplotMatrix(data)
    
    #Boxplot per race category (Marcos)
    #assigning names to columns for easier reading
    colnames(data) <- c("country", "100m", "200m", "400m", "800m", "1500m", "3000m", "marathon")
    
    #convert seconds to minutes in the first three cateogories
    data[,2:4] <- data[,2:4]/60
    #melted data
    meltdata <- melt(data)

    #b) Illustrate variables with different graphs
    p <- ggplot(data = meltdata, aes(x = variable, y = value )) +
        geom_boxplot(aes(fill = variable)) +
        labs(x = "category", y = "Track records (in minutes)")
    boxp <- p + facet_wrap(~variable, scales = "free")
    
    
    #identifying outliets in boxplots for each category:
    
    detect.outlier <- function(x) {
        #note: x should be a column of the data. e.g. data$`100m`
        p <- boxplot(x)
        #numeric outliers
        num_out<- p$out
        
        ind <- c()
        for (i in 1:length(num_out)) {
            ind[i] <- which(x == num_out[i])
        }
        out_country <- data[ind, 1]
        return(out_country)
    }
    
