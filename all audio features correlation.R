install.packages("ggcorrplot")
library(ggcorrplot)

setwd("F:/UM/WQD 7004/Group/Spotify-Popularity-Analysis-and-Music-Segmentation")
df <- read.csv("30000 spotify songs - cleaned.csv")

# 选择数值型的音频特征列
audio_features <- df[sapply(df, is.numeric)]

# 计算相关性矩阵
cor_matrix <- cor(audio_features, use = "complete.obs")

# 绘制相关性热力图
ggcorrplot(cor_matrix,
           method = "square",
           type = "full",   # 画出全矩阵
           lab = TRUE,
           lab_size = 3,
           title = "Correlation Matrix Of Audio Features", # 大写
           colors = c("blue", "white", "red"))



