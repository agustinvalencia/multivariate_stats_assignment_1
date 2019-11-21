data_path <- "data/T1-9.dat"
data <- read.table(data_path)
colnames(data) <- c("country", "100m", "200m", "400m", "800m", "1500m", "3000m", "marathon")

#QUESTION 3b ------------------------
#euclidean distance
records <- as.matrix(data[,2:8])

#center the data
#deviation matrix
centered <- scale(x = records, center = TRUE, scale = FALSE)

euclid_dist <- sqrt((centered) %*% t(centered))

#diagonal
euclid_diag <- sort(diag(euclid_dist), decreasing = TRUE, index.return = TRUE)
#extreme values (top 5)
euclid_extremes <- head(euclid_diag, n = 5)
#extract extreme countries
ind <- head(euclid_extremes$ix, 5)
euclid_extreme_countries <- data[ind,1]
#Sweden's index
SWE_ind <- which(data[,1] == "SWE")
#Sweden's Position
SWE_rank_euclid <- which(euclid_extremes$ix == SWE_ind)

#QUESTION 3c -----------------------
#diagonal of variance covariance matrix
covars <- cov(records)
V <- matrix(0, nrow = ncol(records), ncol = ncol(records))
diag(V) <- diag(covars)
#compute distance
dist3c <- (centered %*% solve(V) %*% t(centered))

#diagonal
diag3c <- sort(diag(dist3c), decreasing = TRUE, index.return = TRUE)
#extreme values (top 5)
extremes3c <- head(diag3c, n = 5)
#extract extreme countries
ind3c<- head(extremes3c$ix, 5)
extreme_countries3c <- data[ind3c,1]
#Sweden's Position
SWE_rank_3c <- which(extremes3c$ix == SWE_ind)


#QUESTION 3d -----------------
dist3d <- (centered %*% solve(covars) %*% t(centered))
#diagonal
diag3d <- sort(diag(dist3d), decreasing = TRUE, index.return = TRUE)
#extreme values (top 5)
extremes3d <- head(diag3d, n = 5)
#extract extreme countries
ind3d<- head(extremes3d$ix, 5)
extreme_countries3d <- data[ind3d,1]
#Sweden's Position
SWE_rank_3d <- which(extremes3d$ix == SWE_ind)

euclid_extreme_countries
extreme_countries3c
extreme_countries3d

SWE_rank_euclid
SWE_rank_3c
SWE_rank_3d

#In this case, extreme countries normally have poor performance in races, but that is not always the case. For some distances definitions, high performance countries like USA and Great Britain appear as extremes as well. So "extremism" is not a measure of how "slow" a country is, but rather how far from the overall mean the country is. 
#By ranking the countries in decreasing order of distance, Sweden's position decreases for the different distances defined in questions b), c), and d) respectively. These distances

library(car)
#for 100m race
center <- colMeans(records[,1:2])
shape1 <- cov(records[,1:2])
shape2 <- V[1:2,1:2]
plot(records[,1:2])
ellipse(center = center, shape = shape1, radius = 1)
ellipse(center = center, shape = shape2, radius = 1)

