# 替换文件路径
data <- read.csv("30000 spotify songs.csv", stringsAsFactors = FALSE)
# 检查是否有完全重复的行
duplicated_rows <- duplicated(data)
sum(duplicated_rows)  # 查看有多少行重复

# 查看重复的具体内容
data[duplicated_rows, ]
# 删除完全重复的行（保留第一条）
data_unique <- data[!duplicated(data), ]
# 只根据某列（如 "track_name"）判断重复
data_unique_track_id <- data[!duplicated(data$track_id), ]
data_unique_track_name <- data[!duplicated(data$track_name), ]
