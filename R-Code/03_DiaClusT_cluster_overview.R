# The script loads the results of the clustering,
# - matches them on the basis of the cluster centroids, 
# - presents significant differences in the clustering features,
# - merges the data sets of men and women, adds T1DM_diagnosis + visit-number (based on caseid and date)

# libraries
library('clue')
library('ggplot2')
library('gridExtra')

# INPUT
data_clustering_female
data_clustering_male
leipzig_women <- contains cluster centroids of kmeans_clustering (women)
leipzig_men <- contains cluster centroids of kmeans_clustering (men)
conditions

#-------------------------------------------------------------------------------
# cluster centroids (euclidean distance)
#-------------------------------------------------------------------------------
# add centroids from ahlqvist reference
ahlqvist_women <- data.frame(
  cluster = c("SIDD", "SIRD", "MOD","MARD"),
  age= c(-0.1929637, 0.3214557, -0.9388278, 0.5980588),
  mean_BMI = c(-0.2415449, 0.5189057, 0.6683606, -0.5854255),
  X11111 = c(0.056469, 1.1801933, -0.1405151, -0.4254893),
  X17856.6 = c(1.8702613, -0.254848, -0.3003478, -0.4582762)
)

ahlqvist_men <- data.frame(
  cluster = c("SIDD", "SIRD", "MOD","MARD"),
  age = c(-0.4017103, 0.4235841, -1.0157681, 0.5031031),
  mean_BMI = c(-0.4284673, 0.5396294, 1.0305317, -0.4776681),
  X11111 = c( -0.1630751, 1.1801031, 0.1343923, -0.4233873),
  X17856.6 = c(1.52185804,-0.39080167,-0.06915764, -0.5367578)
)

# prepare Leipzig dataset
#      leipzig_women
# 1 SIDD, 2 MOD, 3 SIRD, 4 MARD (depending on clustering, check this individually)
colnames(leipzig_women)[1]<-"cluster"
leipzig_women$cluster[1]<-"SIDD"
leipzig_women$cluster[2]<-"MOD"
leipzig_women$cluster[3]<-"SIRD"
leipzig_women$cluster[4]<-"MARD"

#      leipzig_men
# 1 SIDD, 2 MOD, 3 MARD, 4 SIRD (depending on clustering, check this individually)
colnames(leipzig_men)[1]<-"cluster"
leipzig_men$cluster[1]<-"SIDD"
leipzig_men$cluster[2]<-"MOD"
leipzig_men$cluster[3]<-"MARD"
leipzig_men$cluster[4]<-"SIRD"


# compare centroids
centroids_lpz <- leipzig_women[order(leipzig_women$cluster),] # replace genderspecific dataset here
centroids_ahl<-ahlqvist_women[order(ahlqvist_women$cluster),] # replace genderspecific dataset here

euclidean_distance <- function(x, y) {
  sqrt(sum((x - y) ^ 2))
}


# matrix to store distances
dist_matrix <- matrix(nrow = nrow(centroids_lpz), ncol = nrow(centroids_ahl))

# fill matrix
for (i in 1:nrow(centroids_lpz)) {
  for (j in 1:nrow(centroids_ahl)) {
    dist_matrix[i, j] <- euclidean_distance(centroids_lpz[i, -1], centroids_ahl[j, -1])
  }
}

# search minimum distance mapping
matching <- solve_LSAP(as.matrix(dist_matrix))

# data frame to show matched centroids
matched_centroids <- data.frame(
  cluster_lpz = centroids_lpz$cluster,
  cluster_ahl = centroids_ahl$cluster[matching],
  distance = dist_matrix[cbind(1:nrow(centroids_lpz), matching)]
)

centroids_lpz$Clustering <- "Clustering Leipzig"
centroids_ahl$Clustering <- "Clustering Ahlqvist"
centroids_combined <- rbind(centroids_lpz, centroids_ahl)

# example of plotting centroids
a1<-ggplot(centroids_combined, aes(x = age , y = mean_BMI, color = Clustering, shape = factor(cluster))) +
  geom_point(size = 5) +
  labs(title = "Centroid comparison between the two clustering results (Men)", 
       x = "Age", 
       y = "BMI",
       color = "Clustering",
       shape = "Cluster") +
  theme_minimal()+
  theme(legend.position = "none")

b1<-ggplot(centroids_combined, aes(x = age , y = X17856.6, color = Clustering, shape = factor(cluster))) +
  geom_point(size = 5) +
  labs(x = "Age", 
       y = "HbA1c",
       color = "Clustering",
       shape = "Cluster") +
  theme_minimal()+
  theme(legend.position = "none")

c1<-ggplot(centroids_combined, aes(x = age , y = X17856.6, color = Clustering, shape = factor(cluster))) +
  geom_point(size = 5) +
  labs(x = "Age", 
       y = "Ratio TRIG/HDL",
       color = "Clustering",
       shape = "Cluster") +
  theme_minimal()

grid.arrange(a1,b1,c1, ncol=3)

#-------------------------------------------------------------------------------
# merging
#-------------------------------------------------------------------------------
# merge data_clustering_female/ data_clustering_male --> data_clustering

# remove case-ids 
cluster_counts <- data_clustering %>%
  group_by(FallNr) %>%         
  summarize(num_clusters = n_distinct(cluster))
table(cluster_counts$num_clusters)

keep_Fallnr<-cluster_counts[which(cluster_counts$num_clusters == 1),"FallNr"] 
data_clustering <- data_clustering %>% filter(FallNr %in% keep_Fallnr$FallNr)

#-------------------------------------------------------------------------------
# overview of clusters: feature-boxplot
#-------------------------------------------------------------------------------
data_clustering$cluster[which(data_clustering$cluster==1)]<-"1-MARD"    # numbering depends on clustering
data_clustering$cluster[which(data_clustering$cluster==2)]<-"2-MOD"
data_clustering$cluster[which(data_clustering$cluster==3)]<-"3-SIDD"
data_clustering$cluster[which(data_clustering$cluster==4)]<-"4-SIRD"

df_long <- tidyr::gather(data_clustering[,c(5:9)], key = "var", value = "value", -cluster)

df_long$var[df_long$Variable=="age"]<-"Age [years]"
df_long$var[df_long$Variable=="mean_BMI"]<-"BMI [kg/m²]"
df_long$var[df_long$Variable=="X11111"]<-"TRIG/HDL [ ]"
df_long$var[df_long$Variable=="X17856.6"]<-"HbA1c [%]"


# boxplots
ggplot(df_long, aes(x = factor(cluster), y = value, fill = factor(cluster))) +
       geom_boxplot() +
       facet_wrap(~Variable, scales = "free_y",ncol = 2) +
       theme_minimal() +
       theme(
             legend.position = "right",
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             #axis.text.x = element_text(angle = 0, hjust = 1)
             axis.text.x =element_blank()
         ) +
       guides(fill = guide_legend(title = "Cluster"))



# OUTPUT: data_clustering
