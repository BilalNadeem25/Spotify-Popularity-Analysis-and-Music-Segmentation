options(repos = c(CRAN = "https://cloud.r-project.org/"))

install.packages('dplyr')
install.packages('ggplot2')
install.packages('cluster')
install.packages("ggcorrplot")
install.packages("GGally")
install.packages("Rtsne")
install.packages("scales")
install.packages("ClusterR")

library(dplyr)
library(ggplot2)
library(cluster)
library(ggcorrplot)
library(Rtsne)
library(scales)
library(ClusterR)

# Read the cleaned dataset
df <- read.csv('30000 spotify songs - clustering.csv')

features <- c('valence', 'energy')

# Create a dataframe with audio features only
X_df <- df %>%
  select(all_of(features))

# Scale the data with z-score normalization
X_scaled <- scale(X_df)

# Set seed for reproducibility
set.seed(123)

# Perform K-means clustering
kmeans_model <- kmeans(X_scaled, centers = 3, nstart = 20)

# Calculate the Euclidean distance matrix of the features
euc_dist <- dist(X_scaled)

# Calculate the silhouette score
sil_score <- silhouette(kmeans_model$cluster, euc_dist)

# Display the average silhouette width 
cat("Initial kmeans clustering silhouette score with k = 3: ", 
    mean(sil_score[, 3]), "\n\n")

# Visualize Elbow method to determine optimal number of clusters

wss <- numeric(10)

set.seed(123)

n <- 10

for (k in 1:n) {
  # 
  cluster_model <- kmeans(X_scaled, centers = k, nstart = 20)
  # Save the within cluster sum of squares
  wss[k] <- cluster_model$tot.withinss
}

wss_df <- tibble(clusters = 1:n, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters') +
  labs(
    title = "Elbow Method for optimal K",
    y = "Total Within-Cluster Sum of Squares"
  )

scree_plot +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000',10))
  )

cat("According to the Elbow Method plot, the total within cluster sum of square decrease slower after adding more than 6 clusters. Hence, k = 6\n\n")


# Set seed for reproducibility
set.seed(123)

# Perform K-means clustering
kmeans_model <- kmeans(X_scaled, centers = 4, nstart = 25)

# Calculate the Euclidean distance matrix of the features
euc_dist <- dist(X_scaled)

# Calculate the silhouette score
sil_score <- silhouette(kmeans_model$cluster, euc_dist)

# Display the average silhouette width 
cat("Kmeans silhouette score with k = 4: ", mean(sil_score[, 3]), "\n\n")

X_df$cluster <- as.factor(kmeans_model$cluster)

# Perform PCA on scaled data
pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

# Extract first two principal components
pca_df <- as.data.frame(pca_result$x[, 1:2])

# Add cluster labels from KMeans
pca_df$cluster <- as.factor(kmeans_model$cluster)

# Plot PCA
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2.5, alpha = 0.7) +
  labs(
    title = "PCA Visualization of KMeans Clusters",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

