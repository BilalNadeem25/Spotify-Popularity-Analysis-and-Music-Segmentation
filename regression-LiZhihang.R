# 安装必要包（如未安装）
# install packages
install.packages("randomForest")
install.packages("caret")
install.packages("Metrics")

library(randomForest)
library(caret)
library(Metrics)

# 读取数据
# read data
data <- read.csv("spotify songs after cleaning.csv")

# 选取用于建模的数值特征
# choose those columns as features
features <- c("danceability", "energy", "key", "loudness", "mode", 
              "speechiness", "acousticness", "instrumentalness", 
              "liveness", "valence", "tempo", "duration_ms")
target <- "track_popularity"

# 构造公式
# Construction formula
formula <- as.formula(paste("track_popularity ~", paste(features, collapse = " + ")))

# 拆分训练集和测试集（80%训练，20%测试）
# 80% for train，20% for test
set.seed(42)
train_index <- createDataPartition(data$track_popularity, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

# 训练随机森林模型
# Train the random forest model
rf_model <- randomForest(formula, data = train_data, ntree = 100, importance = TRUE)

# 模型预测
# prediction
predictions <- predict(rf_model, newdata = test_data)

# 合并实际值与预测值
# compare the actual and predict for test data
result_df <- data.frame(
  Actual = test_data[[target]],
  Predicted = predictions
)

# 保存为 Excel 文件
# save
write.csv(result_df, file = "actual_vs_predicted_popularity.csv")

cat("The predicted results are compared with the actual results and saved as actual_vs_predicted_popularity.xlsx\n")

# 模型评估
# model evaluate（MSE，RMSE，R^2）
mse_val <- mse(test_data$track_popularity, predictions)
rmse_val <- rmse(test_data$track_popularity, predictions)
r2_val <- R2(predictions, test_data$track_popularity)

cat("Model Evaluation Metrics:\n")
cat("MSE:", round(mse_val, 2), "\n")
cat("RMSE:", round(rmse_val, 2), "\n")
cat("R²:", round(r2_val, 4), "\n\n")

# 特征重要性分析
# Feature importance
cat("Feature Importance:\n")
importance_df <- importance(rf_model)
print(importance_df)

# 可视化特征重要性（可选）
# visualization of the feature importance
varImpPlot(rf_model, main = "Feature Importance (Random Forest)")

