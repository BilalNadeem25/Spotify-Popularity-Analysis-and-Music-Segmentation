options(repos = c(CRAN = "https://cloud.r-project.org/"))

install.packages('naniar')
install.packages('ggplot2')
install.packages('dplyr')
install.packages("tidyr")
install.packages("ggcorrplot")
library(tidyr)            
library(naniar)
library(ggplot2)
library(dplyr)
library(ggcorrplot)

songsdf <- read.csv('30000 spotify songs.csv')

# overview of missingness
vis_miss(songsdf) + labs(title = "Missingness before cleaning")

# filter out records with missing data
songsdf_clean <- songsdf[complete.cases(songsdf), ]

cat("Number of records in dataset after removing missing data: ", nrow(songsdf_clean), "\n")

# overview of missingness in data after cleaning
vis_miss(songsdf_clean) + labs(title = "Missingness after cleaning")

# Keep only those tracks that have unique track-artist pairs
unique_tracks <- songsdf_clean %>%
  distinct(track_name, track_artist) %>%
  nrow()

cat("Number of unique tracks: ", unique_tracks, "\n")

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


audio_loudness_df <- songsdf_clean %>%
  select(loudness)

ggplot(audio_loudness_df, aes(x = "", y = loudness)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of Loudness with outliers", x = "Loudness", y = "Value in dB")

songsdf_clean <- filter_outlier(songsdf_clean, 'loudness')

audio_loudness_df <- songsdf_clean %>%
  select(loudness)

ggplot(audio_loudness_df, aes(x = "", y = loudness)) +
          geom_boxplot(outlier.colour = "red") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Boxplot of Loudness without outliers", x = "Loudness", y = "Value in dB")


audio_tempo_df <- songsdf_clean %>%
  select(tempo)

ggplot(audio_tempo_df, aes(x = "", y = tempo)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of Tempo with outliers", x = "Tempo", y = "Value in bpm")

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

audio_0to1_df <- songsdf_clean[, audio_0to1]

audio_0to1_df_long <- audio_0to1_df %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value")

ggplot(audio_0to1_df_long, aes(x = feature, y = value)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of audio data ranging from 0.0 to 1.0 with outliers", x = "Audio Feature",
       y = "Value")


audio_acousticness_df <- songsdf_clean %>%
  select(acousticness)

ggplot(audio_acousticness_df, aes(x = "", y = acousticness)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of acousticness with outliers", x = "acousticness", y = "Value")

songsdf_clean <- filter_outlier(songsdf_clean, 'acousticness')

audio_acousticness_df <- songsdf_clean %>%
  select(acousticness)

ggplot(audio_acousticness_df, aes(x = "", y = acousticness)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of acousticness without outliers", x = "acousticness", y = "Value")


audio_danceability_df <- songsdf_clean %>%
  select(danceability)

ggplot(audio_danceability_df, aes(x = "", y = danceability)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of danceability with outliers", x = "danceability", y = "Value")

songsdf_clean <- filter_outlier(songsdf_clean, 'danceability')

audio_danceability_df <- songsdf_clean %>%
  select(danceability)

ggplot(audio_danceability_df, aes(x = "", y = danceability)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of danceability without outliers", x = "danceability", y = "Value")


audio_energy_df <- songsdf_clean %>%
  select(energy)

ggplot(audio_energy_df, aes(x = "", y = energy)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of energy with outliers", x = "energy", y = "Value")

songsdf_clean <- filter_outlier(songsdf_clean, 'energy')

audio_energy_df <- songsdf_clean %>%
  select(energy)

ggplot(audio_energy_df, aes(x = "", y = energy)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of energy without outliers", x = "energy", y = "Value")


audio_liveness_df <- songsdf_clean %>%
  select(liveness)

ggplot(audio_liveness_df, aes(x = "", y = liveness)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of liveness with outliers", x = "liveness", y = "Value")

songsdf_clean <- filter_outlier(songsdf_clean, 'liveness')

audio_liveness_df <- songsdf_clean %>%
  select(liveness)

ggplot(audio_liveness_df, aes(x = "", y = liveness)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of liveness without outliers", x = "liveness", y = "Value")


audio_speechiness_df <- songsdf_clean %>%
  select(speechiness)

ggplot(audio_speechiness_df, aes(x = "", y = speechiness)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of speechiness with outliers", x = "speechiness", y = "Value")

songsdf_clean <- filter_outlier(songsdf_clean, 'speechiness')

audio_speechiness_df <- songsdf_clean %>%
  select(speechiness)

ggplot(audio_speechiness_df, aes(x = "", y = speechiness)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of speechiness without outliers", x = "speechiness", y = "Value")


audio_instrumentalness_df <- songsdf_clean %>%
  select(instrumentalness)

ggplot(audio_instrumentalness_df, aes(x = "", y = instrumentalness)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of instrumentalness with outliers", x = "instrumentalness", y = "Value")

songsdf_clean <- filter_outlier(songsdf_clean, 'instrumentalness')

audio_instrumentalness_df <- songsdf_clean %>%
  select(instrumentalness)

ggplot(audio_instrumentalness_df, aes(x = "", y = instrumentalness)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of instrumentalness without outliers", x = "instrumentalness", y = "Value")


audio_0to1_df <- songsdf_clean[, audio_0to1]

audio_0to1_df_long <- audio_0to1_df %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value")

ggplot(audio_0to1_df_long, aes(x = feature, y = value)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of audio data ranging from 0.0 to 1.0 without outliers", x = "Audio Feature",
       y = "Value")

songsdf_clean$vocals <- (1 - songsdf_clean$instrumentalness)

songsdf_clean$speechy_vocals <- (songsdf_clean$speechiness * songsdf_clean$vocals)

songsdf_clean$intensity <- (songsdf_clean$loudness * songsdf_clean$tempo)

songsdf_clean$ambiance <- (songsdf_clean$acousticness * songsdf_clean$instrumentalness)

songsdf_clean$val_energy <- (songsdf_clean$valence * songsdf_clean$energy)

songsdf_clean$vibeness <- (songsdf_clean$danceability * songsdf_clean$energy)

features <- c('danceability','energy','loudness','speechiness','acousticness',
              'instrumentalness','liveness','valence','tempo','vocals','speechy_vocals',
              'intensity','ambiance','val_energy','vibeness')

audio_features <- songsdf_clean[,features]

# Calculate the correlation matrix of audio features
cor_matrix <- cor(audio_features, use = "complete.obs")

# Plot the correlation matrix of audio features
ggcorrplot(cor_matrix,
           method = "square",
           type = "full",   
           lab = TRUE,
           lab_size = 3,
           title = "Correlation Matrix Of Audio Features", # 大写
           colors = c("blue", "white", "red"))


#write.csv(songsdf_clean,'30000 spotify songs - clustering.csv', row.names = FALSE)










