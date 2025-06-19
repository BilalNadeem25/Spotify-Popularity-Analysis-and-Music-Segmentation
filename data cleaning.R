# Cleaning Spotify data: dealing with nulls, dirty data, time transformations and preserving numerical features for modelling purposes
options(repos = c(CRAN = "https://cloud.r-project.org"))
# Loading the necessary packages
install.packages("lubridate")
library(lubridate)
library(dplyr)

# Read raw data
data <- read.csv("30000 spotify songs - cleaned.csv", stringsAsFactors = FALSE)

# Cleaning track_popularity: removing NA and non-numbers
data <- data[!is.na(data$track_popularity), ]
data <- data[!is.na(as.numeric(as.character(data$track_popularity))), ]
data$track_popularity <- as.numeric(as.character(data$track_popularity))

# Cleaning playlist_genre: removes non-categorical data (although this field will not be used later)
data <- data[!grepl("^[0-9.]+$", data$playlist_genre), ]

# Create track_age column
data <- data[!is.na(data$track_album_release_date), ]
data$track_album_release_date <- as.Date(data$track_album_release_date)
data$track_age <- as.integer(floor(time_length(interval(data$track_album_release_date, Sys.Date()), "years")))

# Remove duration_ms outliers (IQR method)
Q1 <- quantile(data$duration_ms, 0.25)
Q3 <- quantile(data$duration_ms, 0.75)
IQR_val <- Q3 - Q1
lower <- Q1 - 1.5 * IQR_val
upper <- Q3 + 1.5 * IQR_val

data <- data[data$duration_ms >= lower & data$duration_ms <= upper, ]

# Selection of features to be used for modelling (removal of genre type features)
selected_features <- c(
  "energy", "loudness", "acousticness", "instrumentalness",
  "tempo", "duration_ms", "track_age", "track_popularity"
)

data_model <- data[, selected_features]

# Save cleaned modelled data
write.csv(data_model, "cleaned_spotify_model_data.csv", row.names = FALSE)

cat("✅ 数The data is cleaned, the genre features are removed, and the results are saved as cleaned_spotify_model_data.csv\n")
