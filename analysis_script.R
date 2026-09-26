# ==============================================================================
# VIRTUAL R DATA ANALYST INTERNSHIP - COMPREHENSIVE PROJECT SCRIPT
# Case Study: Automobile Performance and Pricing Analysis
# ==============================================================================

# ------------------------------------------------------------------------------
# WEEK 1: DATA CLEANING & PRELIMINARY ANALYSIS
# ------------------------------------------------------------------------------

# 1. Load the required dataset (Using built-in MTCCARS as a reliable baseline)
data(mtcars)
car_data <- mtcars

# Convert row names to an actual column for Car Model Names
car_data$car_model <- rownames(car_data)
rownames(car_data) <- NULL

# 2. Inspect the raw data structure
print("--- Raw Data Summary ---")
str(car_data)
summary(car_data)

# 3. Simulate and Handle Missing Values (Introducing real-world complexity)
set.seed(42)
car_data$mpg[c(3, 15)] <- NA    # Injecting simulated missing values
car_data$hp[c(10, 22)] <- NA     # Injecting simulated missing values

print("--- Missing Values Count Before Cleaning ---")
print(colSums(is.na(car_data)))

# Impute missing numerical items using Median values
car_data$mpg[is.na(car_data$mpg)] <- median(car_data$mpg, na.rm = TRUE)
car_data$hp[is.na(car_data$hp)] <- median(car_data$hp, na.rm = TRUE)

# 4. Outlier Detection and Treatment via Interquartile Range (IQR)
q_hp <- quantile(car_data$hp, probs = c(0.25, 0.75))
iqr_hp <- diff(q_hp)
upper_bound <- q_hp + 1.5 * iqr_hp

# Cap extreme values to clean baseline
car_data$hp[car_data$hp > upper_bound] <- upper_bound

# 5. Data Categorization & Type Conversion
car_data$cyl <- as.factor(car_data$cyl) # Cylinder counts converted to categories
car_data$am <- factor(car_data$am, levels = c(0, 1), labels = c("Automatic", "Manual"))

print("--- Cleaned Baseline Summary ---")
summary(car_data)


# ------------------------------------------------------------------------------
# WEEK 2: DATA VISUALIZATION WITH GGPLOT2
# ------------------------------------------------------------------------------
library(ggplot2)

# Chart 1: Mileage Distribution Histogram
ggplot(car_data, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "#3498db", color = "white", alpha = 0.8) +
  labs(title = "Distribution of Vehicle Fuel Efficiency", x = "Miles Per Gallon (MPG)", y = "Frequency") +
  theme_minimal()

# Chart 2: Horsepower vs Price/MPG Scatter Plot
ggplot(car_data, aes(x = hp, y = mpg, color = cyl)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linetype = "dashed") +
  labs(title = "Impact of Horsepower on Fuel Efficiency", x = "Gross Horsepower", y = "Miles Per Gallon (MPG)", color = "Cylinders") +
  theme_minimal()

# Chart 3: Count of Transmission Types by Cylinder Class Bar Chart
ggplot(car_data, aes(x = cyl, fill = am)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(title = "Vehicle Distribution by Cylinder and Transmission Type", x = "Number of Cylinders", y = "Count of Vehicle Models", fill = "Transmission") +
  theme_minimal() +
  scale_fill_manual(values = c("#2ecc71", "#9b59b6"))


# ------------------------------------------------------------------------------
# WEEK 3: STATISTICAL ANALYSIS AND PREDICTIVE MODELING
# ------------------------------------------------------------------------------

# 1. Hypothesis Testing & Correlation Checking
print("--- Correlation Test: Horsepower vs MPG ---")
cor_test_result <- cor.test(as.numeric(car_data$hp), car_data$mpg)
print(cor_test_result)

# 2. Data Splitting (70% Training / 30% Testing Partition)
set.seed(123)
sample_size <- floor(0.70 * nrow(car_data))
train_indices <- sample(seq_len(nrow(car_data)), size = sample_size)

train_set <- car_data[train_indices, ]
test_set  <- car_data[-train_indices, ]

# 3. Model Building: Multiple Linear Regression to Predict MPG
mpg_predictive_model <- lm(mpg ~ hp + wt + cyl, data = train_set)

print("--- Trained Model Statistical Summary ---")
summary(mpg_predictive_model)

# 4. Model Evaluation on Testing Subset
predictions <- predict(mpg_predictive_model, newdata = test_set)
actuals <- test_set$mpg

# Calculate Performance Evaluation Metrics
rmse_value <- sqrt(mean((predictions - actuals)^2))
print(paste("Evaluation Metrics - Test Root Mean Squared Error (RMSE):", round(rmse_value, 4)))

# Print completion flag
print("Data Analysis Project Script Executed Successfully!")