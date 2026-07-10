# # # The script uses the data from the pre-processing and performs the calculation of the 
#   - silhouette index, elbow method,
#   - clustering and 
#   - resampling to calculate the jaccard index, adjusted rand index, mutual information and variation of information

# libraries
library('parallel')
library('factoextra')
library('cluster')
library('ggplot2')
# ji, ari, nmi, vi
library('fossil')
library('clue')
library('mclust')
library('entropy')
library('infotheo')
library('ClustOfVar')

# INPUT
data_preprocessed <- (dataset from DiaClusT_preprocessing.R)

#-------------------------------------------------------------------------------
# scaling (z-transformation) of dataset
#-------------------------------------------------------------------------------
# split gender specific
data_female<-data_preprocessed[which(data_preprocessed$gender=="female"),]
data_male<-data_preprocessed[which(data_preprocessed$gender=="male"),]

data<-data_female    # simply exchange dataset here
data<-na.omit(data)
max_fall <- aggregate(FallNr ~ PatVID, data = data, FUN = function(x) length(unique(x)))

# remove TRIG/HDL ratio over 10
data<-data[which(data$`11111`<=10),]
                      
# exclusion of outliers (according to ahlqvist reference)
remove_outliers <- function(x) {
  mean_val <- mean(x)
  sd_value <- sd(x)
  lower_bound <- mean_val - 5 * sd_value
  upper_bound <- mean_val + 5 * sd_value
  x <- if_else(x >= lower_bound & x <= upper_bound, x, NA_real_)
}

data <- data %>%
  mutate(across(5:8, remove_outliers)) %>%        # last 4 columns contain age, bmi, hba1c, trig/hdl ratio
  drop_na()
                      
# scale
scaled_data<-scale(data[,5:8])


#-------------------------------------------------------------------------------
# silhouette-index
#-------------------------------------------------------------------------------
silhouette_score <- function(k){
  km <- kmeans(scaled_data, centers = k, nstart=25)
  sil <- silhouette(km$cluster, dist(scaled_data))
  mean(sil[, 3])
}

k <- 2:7
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE, main="Dataset Females")
# extract number of clusters with maximum silhouette-index
max_index <- which.max(avg_sil)
optimal_cluster_number<-max_index+1

# decide on max delta
deltas<-diff(avg_sil)
max_decrease_pos <- which.min(deltas)
best_k <- max_decrease_pos + 1


#-------------------------------------------------------------------------------
# elbow-method
#-------------------------------------------------------------------------------
# # Elbow

wcss <- sapply(1:7, function(k) {
  kmeans(scaled_data, centers = k)$tot.withinss
})

# plot wcss values against number of clusters
ggplot() +
  geom_line(aes(x = 1:7, y = wcss), color = "steelblue") +
  geom_point(aes(x = 1:7, y = wcss), color = "red") +
  labs(title = "Elbow Method", x = "Number of Clusters", y = "WCSS") +
  theme_minimal()
#-------------------------------------------------------------------------------
# gap statistics 
#-------------------------------------------------------------------------------
gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 25, K.max = 7, B = 50)
fviz_gap_stat(gap_stat)
optimal_k <- which.max(gap_stat$Tab[, "gap"])
optimal_k

#-------------------------------------------------------------------------------
# kmeans
#-------------------------------------------------------------------------------
# number of clusters
k <- 4
#k-means clustering
kmeans_result<-kmeans(scaled_data, centers=k, iter.max=50, nstart=25)

# clusterplots
fviz_cluster(kmeans_result, data = scaled_data,
             geom= "point",
             ellipse.type = "euclid", # Add concentration ellipses
             #star.plot = TRUE, # Add segments from centroids to items
             main = "Cluster plot Men",
             ggtheme = theme_minimal(),
             show.legend=FALSE)

# add cluster assignments to dataframe
df_with_clusters <- cbind(data_preprocessed, cluster = kmeans_result$cluster)
cluster_means <- aggregate(df_with_clusters[,5:9], by=list(Cluster=df_with_clusters$cluster),mean)

# create datasets containing the cluster centroids for men and women 
# leipzig_women
# leipzig_men

                      
#-------------------------------------------------------------------------------
# jaccard
#-------------------------------------------------------------------------------
num_resamples <- 2000
k <- 4
 
# initial k-means clustering
initial_clustering <- kmeans(scaled_data, centers = k, nstart = 25)
initial_clusters <- initial_clustering$cluster

# compute jaccard similarity between two clusterings
compute_jaccard_similarity <- function(clusters1, clusters2) {
  intersection <- sum(clusters1 == clusters2)
  union <- length(clusters1) + length(clusters2) - intersection
  return(intersection / union)
}

#store jaccard similarities
jaccard_similarities <- numeric(num_resamples)

# resampling + compute jaccard similarities
for (i in 1:num_resamples) {
  # resample  dataset with replacement
  resampled_indices <- sample(1:nrow(scaled_data), replace = TRUE)
  resampled_data <- scaled_data[resampled_indices, ]
  
  # k-means clustering on resampled dataset
  resampled_clustering <- kmeans(resampled_data, centers = k, nstart = 25)
  resampled_clusters <- resampled_clustering$cluster
  
  # hungarian algorithm to align cluster labels
  match <- clue::solve_LSAP(table(initial_clusters, resampled_clusters), maximum = TRUE)
  aligned_resampled_clusters <- match[resampled_clusters]
  
  # jaccard similarity
  jaccard_similarities[i] <- compute_jaccard_similarity(initial_clusters, aligned_resampled_clusters)
}

# distribution of jaccard similarities
mean_jaccard <- mean(jaccard_similarities, na.rm = TRUE)
sd_jaccard <- sd(jaccard_similarities, na.rm = TRUE)
summary_jaccard <- summary(jaccard_similarities)

#plot
hist(jaccard_similarities, breaks = 50, main = "Distribution of Jaccard Similarities", 
     xlab = "Jaccard Similarity", col = "steelblue")

#------------------------------------------------------------------------------
# adjusted Rand-Index
# measures the similarity between two clusterings by considering all pairs of samples 
# and counting pairs that are assigned in the same or different clusters in the predicted
# and true clusterings. It adjusts for chance grouping of elements.
            
# store ARI values
ari_values <- numeric(num_resamples)

# resampling + ARI
for (i in 1:num_resamples) {
 
  resampled_indices <- sample(1:nrow(scaled_data), replace = TRUE)
  resampled_data <- scaled_data[resampled_indices, ]
  
  # k-means clustering on resampled dataset
  resampled_clustering <- kmeans(resampled_data, centers = k, nstart = 25)
  resampled_clusters <- resampled_clustering$cluster
  
  ari_values[i] <- adjustedRandIndex(initial_clusters, resampled_clusters)
}

# distribution 
mean_ari <- mean(ari_values, na.rm = TRUE)
sd_ari <- sd(ari_values, na.rm = TRUE)
summary_ari <- summary(ari_values)

# plot
hist(ari_values, breaks = 50, main = "Distribution of Adjusted Rand Index", 
     xlab = "Adjusted Rand Index", col = "steelblue")

# # # # # # # #
#  Normalized Mutual Information (NMI)
# NMI measures the amount of information obtained about one clustering from the other. 
# It is normalized to account for the size of the clusters, making it a useful measure 
# for comparing clusterings of different sizes.

compute_nmi <- function(clusters1, clusters2) {
  mutual_info <- infotheo::mutinformation(clusters1, clusters2)
  h1 <- entropy::entropy(clusters1)
  h2 <- entropy::entropy(clusters2)
  return(mutual_info / sqrt(h1 * h2))
}

nmi_values <- numeric(num_resamples)


for (i in 1:num_resamples) {
  
  resampled_indices <- sample(1:nrow(scaled_data), replace = TRUE)
  resampled_data <- scaled_data[resampled_indices, ]
  resampled_clustering <- kmeans(resampled_data, centers = k, nstart = 25)
  resampled_clusters <- resampled_clustering$cluster
  nmi_values[i] <- compute_nmi(initial_clusters, resampled_clusters)
}

mean_nmi <- mean(nmi_values, na.rm = TRUE)
sd_nmi <- sd(nmi_values, na.rm = TRUE)
summary_nmi <- summary(nmi_values)

hist(nmi_values, breaks = 50, main = "Distribution of Normalized Mutual Information", 
     xlab = "Normalized Mutual Information", col = "steelblue")

# # # # # # #  -----------------------------------------------------------------
# Variation of Information
#VI measures the amount of information lost and gained in moving from one clustering to another. 
# It is a true metric, meaning it satisfies the triangle inequality and is a measure of the 
#distance between two clusterings

compute_vi <- function(clusters1, clusters2) {
  mutual_info <- infotheo::mutinformation(clusters1, clusters2)
  h1 <- entropy::entropy(clusters1)
  h2 <- entropy::entropy(clusters2)
  return((h1 + h2) - 2 * mutual_info)
}

vi_values <- numeric(num_resamples)

for (i in 1:num_resamples) {
  resampled_indices <- sample(1:nrow(scaled_data), replace = TRUE)
  resampled_data <- scaled_data[resampled_indices, ]
  resampled_clustering <- kmeans(resampled_data, centers = k, nstart = 25)
  resampled_clusters <- resampled_clustering$cluster
  vi_values[i] <- compute_vi(initial_clusters, resampled_clusters)
}


mean_vi <- mean(vi_values, na.rm = TRUE)
sd_vi <- sd(vi_values, na.rm = TRUE)
summary_vi <- summary(vi_values)

hist(vi_values, breaks = 50, main = "Distribution of Variation of Information", 
     xlab = "Variation of Information", col = "steelblue")


# OUTPUT: data_clustering_female, data_clustering_male, leipzig_women, leipzig_men
