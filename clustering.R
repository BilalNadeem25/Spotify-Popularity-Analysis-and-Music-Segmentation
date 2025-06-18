options(repos = c(CRAN = "https://cloud.r-project.org/"))

install.packages('dplyr')
install.packages('ggplot2')
install.packages('cluster')
library(dplyr)
library(ggplot2)
library(cluster)

# Read the cleaned dataset
df <- read.csv('30000 spotify songs - clustering.csv')

features <- c('danceability','energy','key','loudness','mode','speechiness',
              'acousticness','instrumentalness','liveness','valence', 'tempo')

# Create a dataframe with audio features only
X_df <- df %>%
  select(all_of(features))

# Scale the data with z-score normalization
X_scaled <- scale(X_df)

# Set seed for reproducibility
set.seed(123)

# Perform K-means clustering
cluster_df <- kmeans(X_scaled, centers = 3, nstart = 20)

# Calculate the Euclidean distance matrix of the features
euc_dist <- dist(X_scaled)

# Calculate the silhouette score
sil_score <- silhouette(cluster_df$cluster, euc_dist)

# Display the average silhouette width 
mean(sil_score[, 3])

plot(sil_score, border = NA, main = "Silhouette Plot for K-Means Clustering")

# Visualize Elbow method to determine optimal number of clusters

wss <- numeric(12)

set.seed(123)

n <- 12

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
    col = c(rep('#000000',12))
  )

cat("According to the Elbow Method plot, the total within cluster sum of squares
    decrease slower after adding more than 5 clusters. Hence, k = 5\n")


# Set seed for reproducibility
set.seed(123)

# Perform K-means clustering
cluster_df <- kmeans(X_scaled, centers = 5, nstart = 20)

# Calculate the Euclidean distance matrix of the features
euc_dist <- dist(X_scaled)

# Calculate the silhouette score
sil_score <- silhouette(cluster_df$cluster, euc_dist)

# Display the average silhouette width 
mean(sil_score[, 3])

