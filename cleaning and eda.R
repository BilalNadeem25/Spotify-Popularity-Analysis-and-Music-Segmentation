options(repos = c(CRAN = "https://cloud.r-project.org/"))

install.packages('naniar')
install.packages('ggplot2')
install.packages('dplyr')
install.packages("tidyr") 
library(tidyr)            
library(naniar)
library(ggplot2)
library(dplyr)

songsdf <- read.csv('30000 spotify songs.csv')

# overview of missingness
vis_miss(songsdf) + labs(title = "Missingness before cleaning")

# filter out records with missing data
songsdf_clean <- songsdf[complete.cases(songsdf), ]

cat("Number of records in dataset after removing missing data: ", nrow(songsdf_clean), "\n")

# overview of missingness in data after cleaning
vis_miss(songsdf_clean) + labs(title = "Missingness after cleaning")

#data_unique <- songsdf_clean[!duplicated(songsdf_clean), ]
#data_unique_track_id <- data_unique[!duplicated(data_unique$track_id), ]
#final_data <- data_unique_track_id[!duplicated(data_unique_track_id$track_name), ]

#duplicated_rows <- duplicated(final_data)
#cat("Number of duplicate records: ", sum(duplicated_rows), "\n")

# Keep only those tracks that have unique track-artist pairs
unique_tracks <- songsdf_clean %>%
  distinct(track_name, track_artist) %>%
  nrow()

cat("Number of unique tracks: ", unique_tracks, "\n")

songsdf_clean <- songsdf_clean %>%
  distinct(track_name, track_artist, .keep_all = TRUE)

#write.csv(songsdf_clean,'30000 spotify songs - cleaned.csv', row.names = FALSE)

# Create a vector of audio features that range from 0.0 to 1.0
audio_0to1 <- c('danceability','energy','speechiness','acousticness',
                'instrumentalness','liveness','valence')

audio_0to1_df <- songsdf_clean[, audio_0to1]

audio_0to1_df_long <- audio_0to1_df %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value")

ggplot(audio_0to1_df_long, aes(x = feature, y = value)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of audio data ranging from 0.0 to 1.0", x = "Audio Feature",
       y = "Value")

audio_loudness_df <- songsdf_clean %>%
  select(loudness)

ggplot(audio_loudness_df, aes(x = "", y = loudness)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of Loudness with outliers", x = "Loudness", y = "Value in dB")

# determine interquartile range, lower and upper bounds
Q1 <- quantile(songsdf_clean[["loudness"]], 0.25, na.rm = TRUE)
Q3 <- quantile(songsdf_clean[["loudness"]], 0.75, na.rm = TRUE)

IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# filter outliers based on IQR
songsdf_clean <- songsdf_clean %>%
  filter(loudness >= lower_bound & loudness <= upper_bound)

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

# determine interquartile range, lower and upper bounds
Q1 <- quantile(songsdf_clean[["tempo"]], 0.25, na.rm = TRUE)
Q3 <- quantile(songsdf_clean[["tempo"]], 0.75, na.rm = TRUE)

IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# filter outliers based on IQR
songsdf_clean <- songsdf_clean %>%
  filter(tempo >= lower_bound & tempo <= upper_bound)

audio_tempo_df <- songsdf_clean %>%
  select(tempo)

ggplot(audio_tempo_df, aes(x = "", y = tempo)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of Tempo without outliers", x = "Tempo", y = "Value in bpm")













