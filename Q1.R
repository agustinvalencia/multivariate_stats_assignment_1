library(car)
library(reshape)
library(ggplot2)
# Q1

data_path <- "data/T1-9.dat"
data <- read.table(data_path)
#convert seconds to minutes in the first three cateogories
# data[,2:4] <- data[,2:4]/60
colnames(data) <- c("country", "100m", "200m", "400m", "800m", "1500m", "3000m", "marathon")

Q1_a <- function () {
    data <- data[,2:8]
    
    # Means
    column_means <- colMeans(data)
    
    # Variance - Covariance Matrix
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

fit_normals <- function(y, var_name) {
    h <- hist(y, breaks = 20, main = paste("Histogram of", var_name))
    x_normal <- seq(min(y), max(y), length=50)
    y_normal <- dnorm(x_normal, mean = mean(y), sd = sd(y))
    y_normal <- y_normal * diff(h$mids[1:2]) * length(y) 
    lines(x_normal, y_normal, col = "red", lwd = 2)
} 

Q1_b <- function() {
    
    #Boxplot per race category (Marcos)
    #assigning names to columns for easier reading
    colnames(data) <- c("country", "100m", "200m", "400m", "800m", "1500m", "3000m", "marathon")
    
    scatterplotMatrix(data[,2:8])
    
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
    
    
#     Question 2: Relationships between the variables
#     a) Compute the covariance and correlation matrices for the 7 variables. Is there any apparent structure in them? Save these matrices for future use.
    # By analysing the variance covariance matrix, it can be concluded that countries that have a high performance in "shorter" races (100m, 200m and 400m) do not necessarily have high performance in "long distance" races (800m, 1500m, 3000m and marathon). This is coherent to the fact that short and long races require different training and normally the athletes are different in those two categories.
# b) Generate and study the scatterplots between each pair of variables. Any extreme values?
#     c) Explore what other plotting possibilities R offers for multivariate data. Present other (at least two) graphs that you find interesting with respect to this data set.
    for (i in c(2:8)) {
        fit_normals(data[,i], colnames(data)[i])
    }
}

#QUESTION 3


#euclidian distance function (Marcos)

euclidean_dist <- function(data, r) {
    #here X is a column of our data, a vector
    #centering data
    X <- scale(data, center = TRUE, scale = FALSE)
    dist <- c()
    #computing distance
    for (i in 1:length(data)){
        dist[i] <- (abs(X[i])^r)^(1/r)
    }
    
    
    return(dist)
    
}
