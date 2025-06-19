options(repos = c(CRAN = "https://cloud.r-project.org/"))

install.packages("randomForest")
install.packages("caret")
install.packages("Metrics")

library(randomForest)
library(caret)
library(Metrics)

df <- read.csv("30000 spotify songs - cleaned.csv")

features <- c("danceability", "energy", "key", "loudness", "mode", 
              "speechiness", "acousticness", "instrumentalness", 
              "liveness", "valence", "tempo", "duration_ms")

target <- "track_popularity"

formula <- as.formula(paste("track_popularity ~", paste(features, collapse = " + ")))

set.seed(42)
train_index <- createDataPartition(df$track_popularity, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data  <- df[-train_index, ]

rf_model <- randomForest(formula, data = train_data, ntree = 100, importance = TRUE)

predictions <- predict(rf_model, newdata = test_data)

result_df <- data.frame(
  Actual = test_data[[target]],
  Predicted = predictions
)

mse_score <- mse(result_df$Actual, result_df$Predicted)
rmse_score <- rmse(result_df$Actual, result_df$Predicted)
r2_score <- R2(result_df$Actual, result_df$Predicted)

cat("Model Evaluation Metrics:\n")
cat("MSE:", round(mse_score, 2), "\n")
cat("RMSE:", round(rmse_score, 2), "\n")
cat("R²:", round(r2_score, 4), "\n\n")


cat("Feature Importance:\n")
importance_df <- importance(rf_model)
print(importance_df)


varImpPlot(rf_model, main = "Feature Importance (Random Forest)")
