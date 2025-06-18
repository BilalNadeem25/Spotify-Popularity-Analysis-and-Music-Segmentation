options(repos = c(CRAN = "https://cloud.r-project.org/"))

install.packages('dplyr')
install.packages('ggplot2')
install.packages('cluster')
install.packages("ggcorrplot")
install.packages("ClusterR")
install.packages("dbscan")
install.packages("umap")

library(dplyr)
library(ggplot2)
library(cluster)
library(ggcorrplot)
library(ClusterR)
library(dbscan)
library(umap)

# Read the cleaned dataset
df <- read.csv('30000 spotify songs - clustering.csv')

features <- c('danceability','energy','speechiness','acousticness',
              'instrumentalness','liveness','valence')


X_df <- df %>%
  select(all_of(features))

X_scaled <- scale(X_df)

# Calculate the correlation matrix of audio features
cor_matrix <- cor(X_df, use = "complete.obs")

# Plot the correlation matrix of audio features
ggcorrplot(cor_matrix,
           method = "square",
           type = "full",   
           lab = TRUE,
           lab_size = 3,
           title = "Correlation Matrix Of Audio Features", # 大写
           colors = c("blue", "white", "red"))


pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x[, 1:5])  # Use first 5 PCs

db <- dbscan(pca_data, eps = 0.8, minPts = 8)

table(db$cluster)

sil <- silhouette(db$cluster[db$cluster != 0], dist(pca_data[db$cluster != 0, ]))
mean(sil[, 3])

kNNdistplot(pca_data, k = 10)
abline(h = 0.8, col = "red", lty = 2)


set.seed(123)
umap_result <- umap(X_scaled)
umap_data <- as.data.frame(umap_result$layout)

# Run DBSCAN on UMAP output
db <- dbscan(umap_data, eps = 0.8, minPts = 8)
table(db$cluster)

sil <- silhouette(db$cluster[db$cluster != 0], dist(umap_data[db$cluster != 0, ]))
mean(sil[, 3])
