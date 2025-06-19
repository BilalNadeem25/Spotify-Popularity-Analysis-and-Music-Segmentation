# Predicting track_popularity using the Random Forest model and evaluating the results
options(repos = c(CRAN = "https://cloud.r-project.org/"))
# First install the randomForest package
if (!require(randomForest)) install.packages("randomForest")
if (!require(caret)) install.packages("caret")
if (!require(Metrics)) install.packages("Metrics")

# add-on package
library(randomForest)
library(caret)
library(Metrics)

# Read the cleaned data
data <- read.csv("30000 spotify songs - cleaned.csv")

# Creating Feature Columns
features <- c(
  "energy", "loudness","ambiance","duration_ms",
  "track_age","playlist_subgenre_encoded","is_remix",
  "artist_popularity", "artist_track_count"
)
target <- "track_popularity"

# Constructive modelling formulas
formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))

# Divide the training set and test set
set.seed(42)
train_index <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Remove NA from the training set
train_data <- na.omit(train_data)

# Train Random Forest modelling
rf_model <- randomForest(formula, data = train_data, ntree = 100, importance = TRUE)

# Test set prediction
predictions <- predict(rf_model, newdata = test_data)

# Assessment of indicators
mse_val <- mse(test_data[[target]], predictions)
rmse_val <- rmse(test_data[[target]], predictions)
r2_val <- R2(predictions, test_data[[target]])

cat("\n📊 Model Evaluation Metrics:\n")
cat("MSE:", round(mse_val, 2), "\n")
cat("RMSE:", round(rmse_val, 2), "\n")
cat("R-squared:", round(r2_val, 4), "\n")

# Output prediction results
result_df <- data.frame(
  Actual = test_data[[target]],
  Predicted = predictions
)

ggplot(data = NULL, aes(x = result_df$Actual, y = result_df$Predicted)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Predicted vs Actual Track Popularity",
    x = "Actual Popularity",
    y = "Predicted Popularity"
  ) +
  theme_minimal()

# Characteristic importance
cat("\n📌 Feature Importance:\n")
importance_df <- importance(rf_model)
print(importance_df)

# visualisation
varImpPlot(rf_model, main = "Feature Importance (Random Forest)")