options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Install the required R packages
install.packages('naniar')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('dplyr')
install.packages("tidyr")
install.packages("ggcorrplot")
install.packages('cluster')
install.packages("scales")
install.packages("ClusterR")
install.packages("randomForest")
install.packages("caret")
install.packages("Metrics")

# Load the R packages
library(tidyr)            
library(naniar)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(cluster)
library(scales)
library(ClusterR)
library(randomForest)
library(caret)
library(Metrics)

# Read the source data into a dataframe
songsdf <- read.csv('30000 spotify songs.csv')

cat("Number of records in dataset: ", nrow(songsdf), "\n")
cat("Number of columns in dataset: ", ncol(songsdf), "\n")

# Visualize missingness in data
vis_miss(songsdf) + labs(title = "Missingness before cleaning")

# Filter out records with missing data
songsdf_clean <- songsdf[complete.cases(songsdf), ]

cat("Number of records in dataset after removing missing data: ", nrow(songsdf_clean), "\n")

# Visualize missingness in data after cleaning
vis_miss(songsdf_clean) + labs(title = "Missingness after cleaning")

# Identify number of tracks that have unique track-artist pairs
unique_tracks <- songsdf_clean %>%
  distinct(track_name, track_artist) %>%
  nrow()

cat("Number of unique tracks in dataset: ", unique_tracks, "\n")

# Keep only unique track-artist pairs
songsdf_clean <- songsdf_clean %>%
  distinct(track_name, track_artist, .keep_all = TRUE)

# Define a function to remove outliers from audio features
filter_outlier <- function(df, column_name) {
  
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(df[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Define bounds
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Filter the outliers
  df <- df %>%
    filter(.data[[column_name]] >= lower_bound & .data[[column_name]] <= upper_bound)
  
  return(df)
}

# Identify outliers in loudness
audio_loudness_df <- songsdf_clean %>%
  select(loudness)

ggplot(audio_loudness_df, aes(x = "", y = loudness)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of Loudness with outliers", x = "Loudness", y = "Value in dB")

# Filter the outliers in loudness
songsdf_clean <- filter_outlier(songsdf_clean, 'loudness')

audio_loudness_df <- songsdf_clean %>%
  select(loudness)

ggplot(audio_loudness_df, aes(x = "", y = loudness)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of Loudness without outliers", x = "Loudness", y = "Value in dB")

# Identify outliers in tempo
audio_tempo_df <- songsdf_clean %>%
  select(tempo)

ggplot(audio_tempo_df, aes(x = "", y = tempo)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of Tempo with outliers", x = "Tempo", y = "Value in bpm")

# Filter the outliers in tempo
songsdf_clean <- filter_outlier(songsdf_clean, 'tempo')

audio_tempo_df <- songsdf_clean %>%
  select(tempo)

ggplot(audio_tempo_df, aes(x = "", y = tempo)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of Tempo without outliers", x = "Tempo", y = "Value in bpm")


# Create a vector of audio features that range from 0.0 to 1.0
audio_0to1 <- c('danceability','energy','speechiness','acousticness',
                'instrumentalness','liveness','valence')

# Identify outliers in audio features that range from 0.0 to 1.0
audio_0to1_df <- songsdf_clean[, audio_0to1]

audio_0to1_df_long <- audio_0to1_df %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value")

ggplot(audio_0to1_df_long, aes(x = feature, y = value)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of audio data ranging from 0.0 to 1.0 with outliers", x = "Audio Feature",
       y = "Value")

# Filter the outliers in each audio ranging from 0.0 to 1.0 one by one
for (col in audio_0to1) {
  songsdf_clean <- filter_outlier(songsdf_clean, col)
}

# Visualize the boxplots of the audio data after removing outliers
audio_0to1_df <- songsdf_clean[, audio_0to1]

audio_0to1_df_long <- audio_0to1_df %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value")

ggplot(audio_0to1_df_long, aes(x = feature, y = value)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of audio data ranging from 0.0 to 1.0 without outliers", x = "Audio Feature",
       y = "Value")

# Identify outliers in duration_ms
audio_duration_df <- songsdf_clean %>%
  select(duration_ms)

ggplot(audio_duration_df, aes(x = "", y = duration_ms)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of duration with outliers", x = "duration_ms", y = "Value")

# Filter the outliers in duration_ms
songsdf_clean <- filter_outlier(songsdf_clean, 'duration_ms')

audio_duration_df <- songsdf_clean %>%
  select(duration_ms)

ggplot(audio_duration_df, aes(x = "", y = duration_ms)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of duration without outliers", x = "duration_ms", y = "Value")

cat("Number of records in dataset after removing outliers: ", nrow(songsdf_clean), "\n")


# -----------------------------------------Clustering Problem-----------------------------------------------------------


# List all the audio features for a correlation matrix
corr_feature_vec <- c('danceability','energy','loudness','speechiness','acousticness',
              'instrumentalness','liveness','valence','tempo')

corr_features <- songsdf_clean[,corr_feature_vec]

# Calculate the correlation matrix of the audio features
cor_matrix <- cor(corr_features, use = "complete.obs")

# Plot the correlation matrix of the audio features
ggcorrplot(cor_matrix,
           method = "square",
           type = "full",   
           lab = TRUE,
           lab_size = 3,
           title = "Correlation Matrix Of Audio Features",
           colors = c("blue", "white", "red"))

features <- c('danceability','energy','acousticness',
              'instrumentalness','liveness','valence')

df <- songsdf_clean

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

# Feature engineering to improve clustering
df$vocals <- (1 - df$instrumentalness)
df$intensity <- (df$loudness * df$tempo)
df$ambiance <- (df$acousticness * df$instrumentalness)
df$val_energy <- (df$valence * df$energy)
df$vibeness <- (df$danceability * df$energy)

# List all the audio features including engineered ones for a correlation matrix
corr_feature_vec <- c('danceability','energy','loudness','speechiness','acousticness',
                      'instrumentalness','liveness','valence','tempo','vocals',
                      'intensity','ambiance','val_energy','vibeness')

corr_features <- df[,corr_feature_vec]

# Calculate the correlation matrix of the new audio features
cor_matrix <- cor(corr_features, use = "complete.obs")

# Plot the correlation matrix of the new audio features
ggcorrplot(cor_matrix,
           method = "square",
           type = "full",   
           lab = TRUE,
           lab_size = 3,
           title = "Correlation Matrix Of New Audio Features",
           colors = c("blue", "white", "red"))

new_features <- c('val_energy', 'intensity', 'ambiance')

# Create a new dataframe with new audio features
X_df <- df %>%
  select(all_of(new_features))

# Scale the data with z-score normalization
X_scaled <- scale(X_df)

# Visualize Elbow method to determine optimal number of clusters
wss <- numeric(10)
set.seed(123)

n <- 10

for (k in 1:n) {
  cluster_model <- kmeans(X_scaled, centers = k, nstart = 20)
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

cat("According to the Elbow Method plot, the total within cluster sum of squares decreases slower after adding more than 4 clusters. Hence, k = 4\n\n")


# Set seed for reproducibility
set.seed(123)

# Perform K-means clustering
kmeans_model <- kmeans(X_scaled, centers = 4, nstart = 25)

# Calculate the Euclidean distance matrix of the features
euc_dist <- dist(X_scaled)

# Calculate the silhouette score
sil_score <- silhouette(kmeans_model$cluster, euc_dist)

# Display the average silhouette width 
cat("New Kmeans silhouette score with k = 4: ", mean(sil_score[, 3]), "\n\n")

cat("Combining existing features to create new ones and selecting optimal k improved the clustering performance significantly.\n\n")

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

# Store kmeans clusters in the feature dataframe
X_df$cluster <- as.factor(kmeans_model$cluster)

# Calculate the mean value of each cluster
cluster_profile <- X_df %>%
  group_by(cluster) %>%
  summarise(
    mean_val_energy = round(mean(val_energy), 3),
    mean_intensity = round(mean(intensity), 3),
    mean_ambiance = round(mean(ambiance), 3)
  )

# Display the cluster profiles
cluster_profile

# Save the clusters from the feature dataframe to the main dataframe
df$cluster <- X_df$cluster

# Assign a mood to each cluster based on the cluster profiles
df$mood <- case_when(
  df$cluster == 1 ~ "Party",
  df$cluster == 2 ~ "Happy-Chill",
  df$cluster == 3 ~ "Intense",
  df$cluster == 4 ~ "Mellow"
)

# Visualize the number of songs in each mood cluster
cluster_counts <- df %>%
  group_by(mood) %>%
  summarise(count = n())

# Plot a bar chart
ggplot(cluster_counts, aes(x = mood, y = count, fill = mood)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Number of Songs in each Cluster",
    x = "Cluster",
    y = "Number of Songs"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label = count), vjust = -0.5)

# Print the top 10 songs in each mood cluster
calm_df <- df %>%
  filter(mood == "Mellow") %>%
  select(track_name, playlist_subgenre, mood)

head(calm_df, 10)

party_df <- df %>%
  filter(mood == "Party") %>%
  select(track_name, playlist_subgenre, mood)

head(party_df, 10)

feelgood_df <- df %>%
  filter(mood == "Happy-Chill") %>%
  select(track_name, playlist_subgenre, mood)

head(feelgood_df, 10)

intense_df <- df %>%
  filter(mood == "Intense") %>%
  select(track_name, playlist_subgenre, mood)

head(intense_df, 10)


#-----------------------------------------------Regression Problem--------------------------------------------------


# Extract the release year from the release date of the song
df$track_release_year <- as.numeric(as.character(substr(df$track_album_release_date, 1, 4)))

# Identify the top 20 popular artists
top20_artists <- df %>%
  group_by(track_artist) %>%
  summarise(avg_pop = mean(track_popularity, na.rm = TRUE)) %>%
  arrange(desc(avg_pop)) %>%
  slice_head(n = 20)

# Plot a bar chart to visualize the top 20 popular artists 
ggplot(top20_artists, aes(
  x = reorder(track_artist, avg_pop),
  y = avg_pop
)) +
  geom_col(fill = "cadetblue") +
  coord_flip() +
  labs(
    title = "Top 20 Artists by Average Song Popularity",
    x     = "Artist",
    y     = "Average Popularity"
  ) +
  theme_minimal()

# Identify the most popular playlist subgenre
genre_pop <- df %>%
  group_by(playlist_subgenre) %>%
  summarise(avg_pop = mean(track_popularity, na.rm = TRUE)) %>%
  arrange(desc(avg_pop))

ggplot(genre_pop, aes(
  x = reorder(playlist_subgenre, avg_pop),
  y = avg_pop
)) +
  geom_col(fill = "cadetblue") +
  coord_flip() +
  labs(
    title = "Average Song Popularity by Playlist Subgenre",
    x     = "Playlist Subgenre",
    y     = "Average Popularity"
  ) +
  theme_minimal()

# Identify the total number of songs released every year
songs_per_year <- df %>%
  count(track_release_year)

# Plot a line chart to visualize the number of songs released each year
ggplot(songs_per_year, aes(
  x = track_release_year,
  y = n
)) +
  geom_line(group = 1, color = "cadetblue") +
  geom_point() +
  labs(
    title = "Total Number of Songs Released each Year",
    x     = "Year",
    y     = "Number of Songs"
  ) +
  theme_minimal()

cat("\n\nAttempting regression using audio features\n\n")

features <- c(
  "danceability","energy", "loudness", "acousticness", "instrumentalness",
  "speechiness","liveness","valence","tempo", "duration_ms"
)
target <- "track_popularity"

# Construct the modelling formula
formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))

# Divide the data into training set and test set
set.seed(42)
train_index <- sample(seq_len(nrow(df)), size = 0.8 * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Train Random Forest model
rf_model <- randomForest(formula, data = train_data, ntree = 100, importance = TRUE)

# Predict from unseen data
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model
mse_val <- mse(test_data[[target]], predictions)
rmse_val <- rmse(test_data[[target]], predictions)
r2_val <- R2(predictions, test_data[[target]])

cat("\nModel Evaluation Metrics:\n")
cat("MSE:", round(mse_val, 2), "\n")
cat("RMSE:", round(rmse_val, 2), "\n")
cat("R-squared:", round(r2_val, 4), "\n")

# visualize the feature importance
varImpPlot(rf_model, main = "Feature Importance (Random Forest)")

cat("\n\nThe Root Mean Squared Error is quite high, so let us construct meaningful features that can contribute to feature importance\n\n")

# Feature engineering based on EDA to improve regression
current_year <- as.numeric(format(Sys.Date(), "%Y"))
df$track_age <- current_year - df$track_release_year

df$playlist_subgenre_encoded <- as.numeric(factor(df$playlist_subgenre))

df$is_remix <- ifelse(
  grepl("remix", df$track_name, ignore.case = TRUE),
  1,
  0
)

artist_stats <- df %>%
  group_by(track_artist) %>%
  summarise(
    artist_popularity = mean(track_popularity, na.rm = TRUE),
    artist_track_count = n()
  )
df <- left_join(df, artist_stats, by = "track_artist")


new_features <- c(
  "energy", "loudness","ambiance","duration_ms",
  "track_age","playlist_subgenre_encoded","is_remix",
  "artist_popularity", "artist_track_count"
)
target <- "track_popularity"

# Constructive modelling formula
formula <- as.formula(paste(target, "~", paste(new_features, collapse = " + ")))

# Divide the data into training set and test set
set.seed(42)
train_index <- sample(seq_len(nrow(df)), size = 0.8 * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Train Random Forest model
rf_model <- randomForest(formula, data = train_data, ntree = 100, importance = TRUE)

# Predict from unseen data
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model
mse_val <- mse(test_data[[target]], predictions)
rmse_val <- rmse(test_data[[target]], predictions)
r2_val <- R2(predictions, test_data[[target]])

cat("\nModel Evaluation Metrics:\n")
cat("MSE:", round(mse_val, 2), "\n")
cat("RMSE:", round(rmse_val, 2), "\n")
cat("R-squared:", round(r2_val, 4), "\n")

# visualize the feature importance
varImpPlot(rf_model, main = "Feature Importance (Random Forest)")

# Store the predictions and actual target in a dataframe
result_df <- data.frame(
  Actual = test_data[[target]],
  Predicted = predictions
)

# Plot the regression line of best fit
ggplot(data = NULL, aes(x = result_df$Actual, y = result_df$Predicted)) +
  geom_point(alpha = 0.4, color = "cadetblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Predicted vs Actual Track Popularity",
    x = "Actual Popularity",
    y = "Predicted Popularity"
  ) +
  theme_minimal()