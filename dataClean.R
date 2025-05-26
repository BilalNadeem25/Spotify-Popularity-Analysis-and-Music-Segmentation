install.packages('naniar')
install.packages('ggplot2')
install.packages('dplyr')
install.packages("tidyr") 
library(tidyr)            
library(naniar)
library(ggplot2)
library(dplyr)

songsdf <- read.csv('30000 spotify songs.csv')

#missing value overview
vis_miss(songsdf)

#delete all missing values because the ratio of missing value is so low
songsdf_clean <- songsdf[complete.cases(songsdf), ]
vis_miss(songsdf)

# columns what can be detected
vars_to_plot <- c('danceability','energy','loudness',
                  'speechiness','acousticness','liveness',
                  'valence','tempo','duration_ms')

# 1. normalization by using Z-score
scaled_data <- songsdf_clean %>%
  select(all_of(vars_to_plot)) %>%
  mutate(across(everything(), scale))   

# 2. pivot_longer(same as gather() in tidyr)
long_data <- scaled_data %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# 3. visualization
ggplot(long_data, aes(x = variable, y = value)) +
  geom_boxplot(outlier.colour = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of audio features with outliers", y = "")

# filter and detect outliers for each rows
for (var in vars_to_plot) {
  Q1 <- quantile(songsdf_clean[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(songsdf_clean[[var]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  # only remain rows with outliers
  songsdf_clean <- songsdf_clean %>%
    filter(.data[[var]] >= lower & .data[[var]] <= upper)
}

nrow(songsdf)        # original records
nrow(songsdf_clean)  # records after delete

# 检查是否有完全重复的行
duplicated_rows <- duplicated(songsdf_clean)
sum(duplicated_rows)  # 查看有多少行重复

# 查看重复的具体内容
songsdf_clean[duplicated_rows, ]
# 删除完全重复的行（保留第一条）
data_unique <- songsdf_clean[!duplicated(songsdf_clean), ]
# 只根据某列（如 "track_name"）判断重复
data_unique_track_id <- data_unique[!duplicated(data_unique$track_id), ]
final_data <- data_unique_track_id[!duplicated(data_unique_track_id$track_name), ]
write.csv(final_data,'spotify songs after cleaning.csv')

