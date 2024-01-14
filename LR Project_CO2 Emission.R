# Clear R Environment
rm(list=ls(all=TRUE))
graphics.off()
shell("cls")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(MASS)
library(car)
#install.packages("e1071")
# Load the e1071 package
library(e1071)

# Load your data
data <- read.csv("C:/Users/trupt/OneDrive - Clark University/Sem 1/MSDA 3055-03/Project/CO2 Emissions.csv")
str(data)

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

#we dont need these 2 variables, so we are going to drop these:
data <- data[, -c(1, 2)]

# Correlation matrix
correlation_matrix <- cor(data[, sapply(data, is.numeric)])
corrplot(correlation_matrix, method = "circle")

print("Insights from correlation matrix")

print("Fuel Consumption and CO2 Emissions: As expected, there is a strong positive correlation between these two variables (0.865). This means that as fuel consumption increases, CO2 emissions also increase.")

print("Engine Size and CO2 Emissions: There is also a positive correlation between Engine Size and CO2 Emissions (0.800). This makes sense, as larger engines typically burn more fuel and produce more emissions.")

print("Cylinders and CO2 Emissions: The correlation between Cylinders and CO2 Emissions is slightly weaker (0.704) than the correlation between Engine Size and CO2 Emissions. This suggests that the number of cylinders is not quite as important a factor in determining CO2 emissions as engine size.")

print("Fuel Consumption and Cylinders: There is a moderate negative correlation between Fuel Consumption and Cylinders (-0.534). This means that cars with fewer cylinders tend to have better fuel economy.")

print("City Fuel Consumption and Highway Fuel Consumption: There is a strong positive correlation between City Fuel Consumption and Highway Fuel Consumption (0.900). This is to be expected, as cars that are fuel-efficient in the city are likely to be fuel-efficient on the highway as well.")
# Create dummy variables for categorical variables
dummy_data <- data %>%
  mutate_at(vars(Vehicle.Class, Transmission, Fuel.Type), as.factor) %>%
  mutate_if(is.factor, function(x) model.matrix(~x - 1))

#plot.new()
#dev.off()
#corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.7)
#correlation_matrix

# Check for skewness and perform log transformation
skewness <- sapply(dummy_data, function(x) skewness(x))
print(skewness)

print("Variables with moderate positive skewness: Vehicle Class, Transmission, Fuel Type, Cylinders, Fuel Consumption City (L/100 km), Fuel Consumption Hwy (L/100 km). Considering these values, applying a log transformation might help make their distributions more symmetrical, which can improve the linearity assumption in the regression model.")
# Variables to log-transform
variables_to_transform <- c("Vehicle.Class", "Transmission", "Fuel.Type", 
                            "Cylinders", "Fuel.Consumption.City..L.100.km.", 
                            "Fuel.Consumption.Hwy..L.100.km.")

# Convert columns to numeric (if they are not already)
data[variables_to_transform] <- lapply(data[variables_to_transform], as.numeric)

# Check for non-numeric values in the specified columns
non_numeric_values <- sapply(data[variables_to_transform], function(x) any(!is.na(as.numeric(x))))
print(non_numeric_values)

# Log transformation for selected variables (excluding non-numeric values)
data[variables_to_transform[!non_numeric_values]] <- lapply(data[variables_to_transform[!non_numeric_values]], function(x) log(x + 1))
summary(data[variables_to_transform[!non_numeric_values]])

# Assuming 'CO2 Emissions(g/km)' is the target variable
target <- dummy_data$CO2.Emissions.g.km.

# Selecting predictors (excluding the target variable)
predictors <-  dummy_data[, !(names(dummy_data) %in% c("CO2.Emissions.g.km."))]

preproc_predictors <- preProcess(predictors,method=c("center", "scale"))
predictors_scaled <- predict(preproc_predictors, newdata = predictors) 

# Split data into training and testing sets
set.seed(123)  # for reproducibility
trainIndex <- createDataPartition(target, p = 0.7, list = FALSE)
train_data <- predictors_scaled[trainIndex, ]
train_target <- target[trainIndex]
test_data <- predictors_scaled[-trainIndex, ]
test_target <- target[-trainIndex]

# Combine predictors and target into training and testing data frames
train_df <- cbind(train_data, CO2.Emissions.g.km. = train_target)
test_df <- cbind(test_data, CO2.Emissions.g.km. = test_target)

#simple linear regression model
slr_model <- (lm(CO2.Emissions.g.km. ~ Engine.Size.L., data=train_df ))
summary(slr_model)
predictions_slr <- predict(slr_model, newdata = test_df)

# Actual vs Predicted plot
ggplot() +
  geom_point(aes(x = test_target, y = predictions_slr)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs Predicted (Simple linear regression)")

# Fitting the multiple linear regression model
lm_model <- lm(CO2.Emissions.g.km. ~ ., data = train_df)
summary(lm_model)

# Predictions
predictions_lm <- predict(lm_model, newdata = test_df)

# Actual vs Predicted plot
ggplot() +
  geom_point(aes(x = test_target, y = predictions_lm)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs Predicted (MLR)")

# Check for outliers using Cook's distance
cooksd <- cooks.distance(lm_model)
plot(cooksd, pch = "o", main = "Cook's Distance")
abline(h = 4*mean(cooksd, na.rm = TRUE), col = "red")

# Handling outliers using influential points
influential_points <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm = TRUE))])
cleaned_data <- data[!influential_points, ]

# Perform stepwise regression using the 'step' function
# Forward selection
stepwise_model_forward <- step(lm(CO2.Emissions.g.km. ~ ., data = train_df), direction = "forward")
summary(stepwise_model_forward)
predictions_step_forward <- predict(stepwise_model_forward, newdata = test_df)
# Actual vs Predicted plot
ggplot() +
  geom_point(aes(x = test_target, y = predictions_step_forward)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs Predicted (Forward stepwise selection)")

# Backward selection
stepwise_model_backward <- step(lm(CO2.Emissions.g.km. ~ ., data = train_df), direction = "backward")
summary(stepwise_model_backward)
predictions_step_backward <- predict(stepwise_model_backward, newdata = test_df)
# Actual vs Predicted plot
ggplot() +
  geom_point(aes(x = test_target, y = predictions_step_backward)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs Predicted (Backward stepwise selection)")

# Both forward and backward selection
stepwise_model_both <- step(lm(CO2.Emissions.g.km. ~ ., data = train_df), direction = "both")
summary(stepwise_model_both)
predictions_stepwise_model_both <- predict(stepwise_model_both, newdata = test_df)
# Actual vs Predicted plot
ggplot() +
  geom_point(aes(x = test_target, y = predictions_stepwise_model_both)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs Predicted (Both stepwise selection)")

# Evaluate model performance (e.g., RMSE, R-squared, etc.)
rmse_model1 <- sqrt(mean((test_df$CO2.Emissions.g.km. - predictions_slr)^2))
rmse_model2 <- sqrt(mean((test_df$CO2.Emissions.g.km. - predictions_lm)^2))
rmse_step_forward <- sqrt(mean((test_df$CO2.Emissions.g.km. - predictions_step_forward)^2))
rmse_step_backward <- sqrt(mean((test_df$CO2.Emissions.g.km. - predictions_step_backward)^2))
rmse_step_both <- sqrt(mean((test_df$CO2.Emissions.g.km. - predictions_stepwise_model_both)^2))


# Compare RMSE or other metrics
model_comparison <- data.frame(
  Model = c("Simple Linear", "Multiple Linear", "Stepwise Forward","Stepwise Backward","Stepwise Both"),
  RMSE = c(rmse_model1, rmse_model2, rmse_step_forward, rmse_step_backward,rmse_step_both)
)
print(model_comparison)


# Selection of the best model based on RMSE or other metrics
best_model <- model_comparison[which.min(model_comparison$RMSE), ]

# Display the best model
print(best_model)

# Detect outliers using studentized residuals
studentized_residuals <- rstudent(lm_model)  # Replace 'model2' with your chosen model
outliers <- which(abs(studentized_residuals) > 3)  # Identify outliers using a threshold (e.g., 3)

# Plot studentized residuals against predicted values
plot(predict(lm_model), studentized_residuals, 
     xlab = "Predicted Values", ylab = "Studentized Residuals",
     main = "Plot of Studentized Residuals vs Predicted Values")

# Highlight outliers beyond a threshold (e.g., 3)
threshold <- 3
outliers <- which(abs(studentized_residuals) > threshold)

# Mark outliers on the plot
points(predict(lm_model)[outliers], studentized_residuals[outliers], col = "red", pch = 16)

# Test for non-linearity, heteroscedasticity, or outliers
# Plotting Residuals vs Fitted Values
plot(lm_model, which = 1, col = "blue", pch = 16, main = "Residuals vs Fitted")
# Plotting Normal Q-Q Plot
plot(lm_model, which = 2, col = "blue", pch = 16, main = "Normal Q-Q Plot")
# Plotting Scale-Location Plot
plot(lm_model, which = 3, col = "blue", pch = 16, main = "Scale-Location")
# Plotting Residuals vs Leverage
plot(lm_model, which = 5, col = "blue", pch = 16, main = "Residuals vs Leverage")
# Plotting Cook's distance
plot(lm_model, which = 4, col = "blue", pch = 16, main = "Cook's distance plot")

# Extracting predictors used in the lm_model
selected_predictors <- names(lm_model$coefficients)[-1]
# Subsetting the data to include only the selected predictors
selected_data <- train_df[, c(selected_predictors[selected_predictors %in% colnames(train_df)], "CO2.Emissions.g.km.")]
# Calculate correlation matrix
cor_matrix <- cor(selected_data, use = "pairwise.complete.obs")
# Print correlation matrix
print("Correlation Matrix:")
print(round(cor_matrix, 2))
# Identify highly correlated features
high_correlation <- findCorrelation(cor_matrix, cutoff = 0.9)
# Print highly correlated features
print("Highly correlated features:")
print(selected_predictors[high_correlation])

# Get residuals from lm_model
residuals_lm <- residuals(lm_model)
# Plot residuals vs each independent variable
par(mfrow = c(1, 2))  # Set up a 1x2 grid for subplots
for (predictor in names(lm_model$coefficients)[-1]) {
  if (predictor %in% colnames(train_df)) {
    plot(train_df[, predictor], residuals_lm,
         xlab = predictor, ylab = "Residuals",
         main = paste("Residuals vs", predictor))
  } else {
    warning(paste("Column", predictor, "not found in train_df. Skipping plot."))
  }
}
# Reset the plotting layout to default
par(mfrow = c(1, 1))

anova(lm_model)
