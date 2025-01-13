library(caret)
library(randomForest)
library(ggplot2)
library(gridExtra)

#Run the Environmental script to obtain modelbio and modelenv

# Step 1: Data Preparation
# Ensure df_wide contains the necessary columns, preprocess if needed
 # Create the red_tide_occurrences column
modelbio$red_tide_occurrences <- 0

 # Sum the occurrences of each red tide species
for (i in 1:nrow(df_wide)) {
  modelbio$red_tide_occurrences[i] <- sum(df_wide[2:ncol(df_wide)][i, -1])  # Assuming the first column is not a species column
}


# Step 2: Feature Selection
# Select the relevant predictor variables (e.g., temperature, salinity, and dissolved oxygen)
modeldata = data.frame(modelbio, modelenv)

colnames(modeldata) = c(colnames(modelbio) , colnames(modelenv))

modeldata = na.omit(modeldata) #remove potential NA values from environmental data

#2.1 keep only red_tide_sightings
modeldata = modeldata[,(ncol(modeldata)-ncol(df_env_significant)):ncol(modeldata)]

#2.2 clean up names
# Function to remove characters after the last underscore
remove_after_last_space <- function(x) {
  last_space <- max(gregexpr(" ", x)[[1]])
  if (last_space > 0) {
    substr(x, 1, last_space - 1)
  } else {
    x
  }
}

# Apply the function to the vector
colnames(modeldata) <- sapply(colnames(modeldata), remove_after_last_space)

colnames(modeldata) = gsub(" ", "_", colnames(modeldata)) #get rid of spaces
colnames(modeldata)

# Step 3: Train-Test Split
# Split the data into training and testing datasets

set.seed(NULL)  # For reproducibility
train_index <- createDataPartition(modeldata$red_tide_occurrences, p = 0.8, list = FALSE)
train_data <- modeldata[train_index, ]
test_data <- modeldata[-train_index, ]

# Step 4: Model Selection
# Choose a model for prediction (e.g., random forest)




model <- randomForest(red_tide_occurrences ~ 
                        BOD5+
                        Orthophosphate_Phosphorus+
                        pH+
                        Salinity+
                        Silica+
                        Suspended_solids+
                        Temperature+
                        Total_phosphorus+
                        Turbidity+
                        Volatile_suspended_solids, 
                      data = train_data)

# Step 5: Model Training
# Train the model using the training dataset
trained_model <- model



# Step 6: Model Evaluation
# Evaluate the model using the testing dataset
predictions <- predict(trained_model, newdata = test_data)

# Calculate evaluation metrics
mae <- mean(abs(predictions - test_data$red_tide_occurrences))
mse <- mean((predictions - test_data$red_tide_occurrences)^2)
rmse <- sqrt(mse)
r_squared <- 1 - sum((test_data$red_tide_occurrences - predictions)^2) / sum((test_data$red_tide_occurrences - mean(test_data$red_tide_occurrences))^2)

# Print the evaluation metrics
cat("Mean Absolute Error:", mae, "\n")
cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Step 7: Model Tuning (if necessary)
# Perform hyperparameter tuning or other optimization techniques as needed

#if left as is, the model is a poor fit

# Additional Steps:
# Once you are satisfied with the model performance, you can use it for future predictions

#Calculating the mean environmental factors throughout the years

significant_factors = which(significant_factors == TRUE)
env4 = env3[significant_factors + 1]

env4_means = colMeans(env4)

env4_means = as.vector(env4_means)

new_data <- rbind(env4_means)
colnames(new_data) = colnames(train_data)[-1]
new_data = as.data.frame(new_data)


new_prediction <- predict(trained_model, newdata = new_data)
cat("New Prediction:", new_prediction, "\n")

#Prediction for temperature increase
red_tide_predict = c(new_prediction)
Temperature = unlist(c(new_data$Temperature))

for (i in 1:312){ #frm 2022 to 2050 at 0.0452 according to our previous results
  new_data$Temperature = new_data$Temperature + as.vector(unlist(subset(slopes, Variable == "Temperature (°C)")[3]/12))
  new_prediction <- predict(trained_model, newdata = new_data)
  red_tide_predict = append(red_tide_predict, new_prediction)
  Temperature = append(Temperature, new_data$Temperature)
}

Future = data.frame("Temperature" = as.numeric(Temperature), red_tide_predict)

RT_All_Env = ggplot(Future, aes(x = Temperature, y = red_tide_predict)) +
  geom_line() +
  labs(x = "Temperature", y = "Predicted red tides in a month") +
  theme_minimal()
RT_All_Env

######Simplificationn
#But perhaps the model is too complicated and overfitted. What if we look at one species, like Noctiluca scintillans
modeldata = data.frame(modelbio$`Noctiluca scintillans`, modelenv)
colnames(modeldata) = c("Noctiluca_scintillans" , colnames(modelenv))


#2.2 clean up names
# Apply the function to the vector
colnames(modeldata) <- sapply(colnames(modeldata), remove_after_last_space)

colnames(modeldata) = gsub(" ", "_", colnames(modeldata)) #get rid of spaces
colnames(modeldata)

# Step 3: Train-Test Split
# Split the data into training and testing datasets

set.seed(NULL)  # For reproducibility
train_index <- createDataPartition(modeldata$Noctiluca_scintillans, p = 0.8, list = FALSE)
train_data <- modeldata[train_index, ]
test_data <- modeldata[-train_index, ]

# Step 4: Model Selection
# Choose a model for prediction (e.g., random forest)

model <- randomForest(Noctiluca_scintillans ~ 
                        BOD5+
                        Orthophosphate_Phosphorus+
                        pH+
                        Salinity+
                        Silica+
                        Suspended_solids+
                        Temperature+
                        Total_phosphorus+
                        Turbidity+
                        Volatile_suspended_solids, 
                      data = train_data)

# Step 5: Model Training
# Train the model using the training dataset
trained_model <- model

# Step 6: Model Evaluation
# Evaluate the model using the testing dataset
predictions <- predict(trained_model, newdata = test_data)

# Calculate evaluation metrics
mae <- mean(abs(predictions - test_data$Noctiluca_scintillans))
mse <- mean((predictions - test_data$Noctiluca_scintillans)^2)
rmse <- sqrt(mse)
r_squared <- 1 - sum((test_data$Noctiluca_scintillans - predictions)^2) / sum((test_data$Noctiluca_scintillans - mean(test_data$Noctiluca_scintillans))^2)

# Print the evaluation metrics
cat("Mean Absolute Error:", mae, "\n")
cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Step 7: Model Tuning (if necessary)
# Perform hyperparameter tuning or other optimization techniques as needed

#

# Additional Steps:
# Once you are satisfied with the model performance, you can use it for future predictions
env4 = env3[significant_factors + 1]
env4_means = colMeans(env4)

env4_means = as.vector(env4_means)

new_data <- rbind(env4_means)
colnames(new_data) = colnames(train_data)[-1]
new_data = as.data.frame(new_data)


new_prediction <- predict(trained_model, newdata = new_data)
cat("New Prediction:", new_prediction, "\n")

#Prediction for temperature increase
red_tide_predict = c(new_prediction)
Temperature = c(new_data$Temperature)

for (i in 1:312){ #frm 2022 to 2050 at increased temp
  new_data$Temperature = new_data$Temperature + as.vector(unlist(subset(slopes, Variable == "Temperature (°C)")[3]/12))
  new_prediction <- predict(trained_model, newdata = new_data)
  red_tide_predict = append(red_tide_predict, new_prediction)
  Temperature = append(Temperature, new_data$Temperature)
}

Future = data.frame("Temperature" = as.numeric(Temperature), red_tide_predict)

NScint_All_Env = ggplot(Future, aes(x = Temperature, y = red_tide_predict)) +
  geom_line() +
  labs(x = "Temperature", y = "Predicted red tides in a month") +
  theme_minimal()
NScint_All_Env

##########################
#Simplification: Tolo harbour
#Also, some areas display very few red tides(less than one per year in total, so maybe look at only tolo harbour)
# Step 1: Data Preparation
# Ensure df_wide contains the necessary columns, preprocess if needed
# Create the red_tide_occurrences column
modelbio$red_tide_occurrences <- 0

# Sum the occurrences of each red tide species
for (i in 1:nrow(df_wide)) {
  modelbio$red_tide_occurrences[i] <- sum(df_wide[2:ncol(df_wide)][i, -1])  # Assuming the first column is not a species column
}


# Step 2: Feature Selection
# Select the relevant predictor variables (e.g., temperature, salinity, and dissolved oxygen)
modeldata = data.frame(modelbio, modelenv)

colnames(modeldata) = c(colnames(modelbio) , colnames(modelenv))

modeldata = na.omit(modeldata) #remove potential NA values from environmental data

#subset for Tolo Harbour & Channel
modeldata$WCZ = substr(modeldata$Sample, start = 10, stop = length(modeldata$Sample))

modeldata = subset(modeldata, WCZ == "Tolo Harbour & Channel")
modeldata = modeldata[1:ncol(modeldata)-1]

#2.1 keep only red_tide_sightings
modeldata = modeldata[,(ncol(modeldata)-ncol(df_env_significant)):ncol(modeldata)]

#2.2 clean up names
# Function to remove characters after the last underscore
remove_after_last_space <- function(x) {
  last_space <- max(gregexpr(" ", x)[[1]])
  if (last_space > 0) {
    substr(x, 1, last_space - 1)
  } else {
    x
  }
}

# Apply the function to the vector
colnames(modeldata) <- sapply(colnames(modeldata), remove_after_last_space)

colnames(modeldata) = gsub(" ", "_", colnames(modeldata)) #get rid of spaces
colnames(modeldata)

# Step 3: Train-Test Split
# Split the data into training and testing datasets

set.seed(NULL)  # For reproducibility
train_index <- createDataPartition(modeldata$red_tide_occurrences, p = 0.8, list = FALSE)
train_data <- modeldata[train_index, ]
test_data <- modeldata[-train_index, ]

# Step 4: Model Selection
# Choose a model for prediction (e.g., random forest)

model <- randomForest(red_tide_occurrences ~ 
                        BOD5+
                        Orthophosphate_Phosphorus+
                        pH+
                        Salinity+
                        Silica+
                        Suspended_solids+
                        Temperature+
                        Total_phosphorus+
                        Turbidity+
                        Volatile_suspended_solids, 
                      data = train_data)


# Step 5: Model Training
# Train the model using the training dataset
trained_model <- model



# Step 6: Model Evaluation
# Evaluate the model using the testing dataset
predictions <- predict(trained_model, newdata = test_data)

# Calculate evaluation metrics
mae <- mean(abs(predictions - test_data$red_tide_occurrences))
mse <- mean((predictions - test_data$red_tide_occurrences)^2)
rmse <- sqrt(mse)
r_squared <- 1 - sum((test_data$red_tide_occurrences - predictions)^2) / sum((test_data$red_tide_occurrences - mean(test_data$red_tide_occurrences))^2)

# Print the evaluation metrics
cat("Mean Absolute Error:", mae, "\n")
cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Step 7: Model Tuning (if necessary)
# Perform hyperparameter tuning or other optimization techniques as needed

#It's a good fit, but it doesn't explain much

# Additional Steps:
# Once you are satisfied with the model performance, you can use it for future predictions
#get the average data for the last year (2022) of the environmental data

env4 = env3
env4$WCZ = substr(env4$Sample, start = 10, stop = length(env4$Sample))
env4 = subset(env4, env4$WCZ == "Tolo Harbour & Channel")
env4$Year = substr(env4$Sample, start = 5, stop = 8)
env4 = subset(env4, env4$Year == 2022)

significant_factors = which(significant_factors == TRUE)
env4 = env4[significant_factors + 1]
env4_means = colMeans(env4)

env4_means = as.vector(env4_means)

new_data <- rbind(env4_means)
colnames(new_data) = colnames(train_data)[-1]
new_data = as.data.frame(new_data)


new_prediction <- predict(trained_model, newdata = new_data)
cat("New Prediction:", new_prediction, "\n")

#Prediction for temperature increase
red_tide_predict = c(new_prediction)
Temperature = c(new_data$Temperature)

for (i in 1:312){ #frm 2022 to 2050 at .01 degree Celcius per month (doesn't make sense, but just to check)
  new_data$Temperature = new_data$Temperature + as.vector(unlist(subset(slopes, Variable == "Temperature (°C)")[3]/12))
  new_prediction <- predict(trained_model, newdata = new_data)
  red_tide_predict = append(red_tide_predict, new_prediction)
  Temperature = append(Temperature, new_data$Temperature)
}

Future = data.frame("Temperature" = as.numeric(Temperature), red_tide_predict)

RT_TH_all_env = ggplot(Future, aes(x = Temperature, y = red_tide_predict)) +
  geom_line() +
  labs(x = "Temperature", y = "Red Tide Prediction") +
  theme_minimal() +
  ggtitle("Red Tide Sightings by temperature in Tolo Harbour") +
  theme(plot.title = element_text(hjust = 0.5))
RT_TH_all_env


####
#Slim it down to just predicting Noctiluca scintillas in Tolo harbour
modeldata = data.frame(modelbio, modelenv)

colnames(modeldata) = c(colnames(modelbio) , colnames(modelenv))

modeldata = na.omit(modeldata) #remove potential NA values from environmental data

#subset for Tolo Harbour & Channel
modeldata$WCZ = substr(modeldata$Sample, start = 10, stop = length(modeldata$Sample))

modeldata = subset(modeldata, WCZ == "Tolo Harbour & Channel")
modeldata = modeldata[1:ncol(modeldata)-1]

#2.1 keep only Noctiluca scintillans
modeldata = data.frame(modeldata$`Noctiluca scintillans`, modeldata[,(ncol(modeldata)-(ncol(df_env_significant))):ncol(modeldata)])
modeldata = modeldata[-2]

#2.2 clean up names

colnames(modeldata) = c("Noctiluca_scintillans" , colnames(modelenv))
# Apply the function to the vector
colnames(modeldata) <- sapply(colnames(modeldata), remove_after_last_space)

colnames(modeldata) = gsub(" ", "_", colnames(modeldata)) #get rid of spaces
colnames(modeldata)

# Step 3: Train-Test Split
# Split the data into training and testing datasets

set.seed(NULL)  # For reproducibility
train_index <- createDataPartition(modeldata$Noctiluca_scintillans, p = 0.8, list = FALSE)
train_data <- modeldata[train_index, ]
test_data <- modeldata[-train_index, ]

# Step 4: Model Selection
# Choose a model for prediction (e.g., random forest)

model <- randomForest(Noctiluca_scintillans ~ 
                        BOD5+
                        Orthophosphate_Phosphorus+
                        pH+
                        Salinity+
                        Silica+
                        Suspended_solids+
                        Temperature+
                        Total_phosphorus+
                        Turbidity+
                        Volatile_suspended_solids, 
                      data = train_data)


# Step 5: Model Training
# Train the model using the training dataset
trained_model <- model

# Step 6: Model Evaluation
# Evaluate the model using the testing dataset
predictions <- predict(trained_model, newdata = test_data)

# Calculate evaluation metrics
mae <- mean(abs(predictions - test_data$Noctiluca_scintillans))
mse <- mean((predictions - test_data$Noctiluca_scintillans)^2)
rmse <- sqrt(mse)
r_squared <- 1 - sum((test_data$Noctiluca_scintillans - predictions)^2) / sum((test_data$Noctiluca_scintillans - mean(test_data$Noctiluca_scintillans))^2)

# Print the evaluation metrics
cat("Mean Absolute Error:", mae, "\n")
cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Step 7: Model Tuning (if necessary)
# Perform hyperparameter tuning or other optimization techniques as needed


# Additional Steps:
# Once you are satisfied with the model performance, you can use it for future predictions
#get the average data for the last year (2022) of the environmental data
env4 = env3
env4$WCZ = substr(env4$Sample, start = 10, stop = length(env4$Sample))
env4 = subset(env4, env4$WCZ == "Tolo Harbour & Channel")
env4$Year = substr(env4$Sample, start = 5, stop = 8)
env4 = subset(env4, env4$Year == 2022)

env4 = env4[significant_factors + 1]
env4_means = colMeans(env4)

env4_means = as.vector(env4_means)

new_data <- rbind(env4_means)
colnames(new_data) = colnames(train_data)[-1]
new_data = as.data.frame(new_data)
store_new_data = new_data

new_prediction <- predict(trained_model, newdata = new_data)

cat("New Prediction:", new_prediction, "\n")

#Prediction for temperature increase
red_tide_predict = c(new_prediction)
Temperature = c(new_data$Temperature)

for (i in 1:336){ #frm 2022 to 2050 at .01 degree Celcius per month (doesn't make sense, but just to check)
  new_data$Temperature = new_data$Temperature + as.vector(unlist(subset(slopes, Variable == "Temperature (°C)")[3]/12))
  new_prediction <- predict(trained_model, newdata = new_data)
  red_tide_predict = append(red_tide_predict, new_prediction)
  Temperature = append(Temperature, new_data$Temperature)
}

Future = data.frame("Temperature" = as.numeric(Temperature), red_tide_predict)

ggplot(Future, aes(x = Temperature, y = red_tide_predict)) +
  geom_line() +
  labs(x = "Temperature", y = "Noctiluca scintillans Prediction") +
  theme_minimal() +
  ggtitle("Noctiluca scintillans Red Tide Sightings by temperature in Tolo Harbour") +
  theme(plot.title = element_text(hjust = 0.5))

mean(as.numeric(unlist(modeldata[1])))
########

#Multiple models learning for the previous model
rvalues = c()
Futures = c(Future[1])

for (i in 1:100){
  set.seed(NULL)  # For reproducibility
  train_index <- createDataPartition(modeldata$Noctiluca_scintillans, p = 0.8, list = FALSE)
  train_data <- modeldata[train_index, ]
  test_data <- modeldata[-train_index, ]
  
  # Step 4: Model Selection
  # Choose a model for prediction (e.g., random forest)
  
  model <- randomForest(Noctiluca_scintillans ~ 
                          BOD5+
                          Orthophosphate_Phosphorus+
                          pH+
                          Salinity+
                          Silica+
                          Suspended_solids+
                          Temperature+
                          Total_phosphorus+
                          Turbidity+
                          Volatile_suspended_solids, 
                        data = train_data)
  
  
  # Step 5: Model Training
  # Train the model using the training dataset
  trained_model <- model
  
  # Step 6: Model Evaluation
  # Evaluate the model using the testing dataset
  predictions <- predict(trained_model, newdata = test_data)
  
  # Calculate evaluation metrics
  mae <- mean(abs(predictions - test_data$Noctiluca_scintillans))
  mse <- mean((predictions - test_data$Noctiluca_scintillans)^2)
  rmse <- sqrt(mse)
  r_squared <- 1 - sum((test_data$Noctiluca_scintillans - predictions)^2) / sum((test_data$Noctiluca_scintillans - mean(test_data$Noctiluca_scintillans))^2)
  
  rvalues = append(rvalues, r_squared) #store for later
  
  new_data = store_new_data
  
  new_prediction <- predict(trained_model, newdata = new_data)
  
  cat("New Prediction:", new_prediction, "\n")
  
  #Prediction for temperature increase
  red_tide_predict = c(new_prediction)
  Temperature = c(new_data$Temperature)
  
  for (i in 1:336){ #frm 2022 to 2050 at .01 degree Celcius per month (doesn't make sense, but just to check)
    new_data$Temperature = new_data$Temperature + as.vector(unlist(subset(slopes, Variable == "Temperature (°C)")[3]/12))
    new_prediction <- predict(trained_model, newdata = new_data)
    red_tide_predict = append(red_tide_predict, new_prediction)
    Temperature = append(Temperature, new_data$Temperature)
  }
  
  Future = data.frame("Temperature" = as.numeric(Temperature), red_tide_predict)
  Futures = data.frame(Futures, Future$red_tide_predict)
}

rsquare_Temperature = mean(rvalues)
Temp_means = rowMeans(Futures[-1])
Temperatures = Futures$Temperature

predictions = Futures[-1]

rsquare_BOD5 = mean(rvalues)

Years = seq(1:length(red_tide_predict))/12
# Calculate the means and confidence intervals for each row
Tempmean_predictions <- rowMeans(predictions)
Tempci <- t(apply(predictions, 1, function(x) t.test(x, conf.int = TRUE)$conf.int))

# Create a data frame with the means, lower and upper CIs, and Years
Tempdata <- data.frame(Years, Tempmean_predictions, lower_ci = Tempci[,1], upper_ci = Tempci[,2])

# Plot the means with confidence intervals
ggplot(Tempdata, aes(x = Years, y = Tempmean_predictions)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.3) +
  xlab("Years") +
  ylab("Predictions")

###########################
#Bod5 prediction
new_data = store_new_data

new_prediction <- predict(trained_model, newdata = new_data)


cat("New Prediction:", new_prediction, "\n")

#Prediction for temperature increase
new_data$BOD5 = as.numeric(new_data$BOD5)
red_tide_predict = c(new_prediction)
BOD5 = c(new_data$BOD5)

for (i in 1:336){ #frm 2022 to 2050 at .01 degree Celcius per month (doesn't make sense, but just to check)
  new_data$BOD5 = new_data$BOD5 + as.vector(unlist(subset(slopes, Variable == "BOD5 (mg/L)")[3]/12)) # average decline of BOD5 per month
  new_prediction <- predict(trained_model, newdata = new_data)
  red_tide_predict = append(red_tide_predict, new_prediction)
  BOD5 = append(BOD5, new_data$BOD5)
}

Future = data.frame("BOD5" = as.numeric(BOD5), red_tide_predict)

ggplot(Future, aes(x = BOD5, y = red_tide_predict)) +
  geom_line() +
  labs(x = "BOD5", y = "Noctiluca scintillans Prediction") +
  theme_minimal() +
  ggtitle("Noctiluca scintillans Red Tide Sightings by BOD5 in Tolo Harbour") +
  theme(plot.title = element_text(hjust = 0.5))

mean(as.numeric(unlist(modeldata[1])))
########

#Multiple models learning for the previous model
rvalues = c()
Futures = c(Future[1])

for (i in 1:100){
  set.seed(NULL)  # For reproducibility
  train_index <- createDataPartition(modeldata$Noctiluca_scintillans, p = 0.8, list = FALSE)
  train_data <- modeldata[train_index, ]
  test_data <- modeldata[-train_index, ]
  
  # Step 4: Model Selection
  # Choose a model for prediction (e.g., random forest)
  
  model <- randomForest(Noctiluca_scintillans ~ 
                          BOD5+
                          Orthophosphate_Phosphorus+
                          pH+
                          Salinity+
                          Silica+
                          Suspended_solids+
                          Temperature+
                          Total_phosphorus+
                          Turbidity+
                          Volatile_suspended_solids, 
                        data = train_data)
  
  
  # Step 5: Model Training
  # Train the model using the training dataset
  trained_model <- model
  
  # Step 6: Model Evaluation
  # Evaluate the model using the testing dataset
  predictions <- predict(trained_model, newdata = test_data)
  
  # Calculate evaluation metrics
  mae <- mean(abs(predictions - test_data$Noctiluca_scintillans))
  mse <- mean((predictions - test_data$Noctiluca_scintillans)^2)
  rmse <- sqrt(mse)
  r_squared <- 1 - sum((test_data$Noctiluca_scintillans - predictions)^2) / sum((test_data$Noctiluca_scintillans - mean(test_data$Noctiluca_scintillans))^2)
  
  rvalues = append(rvalues, r_squared) #store for later
  
  new_data = store_new_data
  
  new_prediction <- predict(trained_model, newdata = new_data)
  
  cat("New Prediction:", new_prediction, "\n")
  
  #Prediction for temperature increase
  red_tide_predict = c(new_prediction)
  BOD5 = c(new_data$BOD5)
  
  for (i in 1:336){ #frm 2022 to 2050 at .01 degree Celcius per month (doesn't make sense, but just to check)
    new_data$BOD5 = new_data$BOD5 + as.vector(unlist(subset(slopes, Variable == "BOD5 (mg/L)")[3]/12)) 
    new_prediction <- predict(trained_model, newdata = new_data)
    red_tide_predict = append(red_tide_predict, new_prediction)
    BOD5 = append(BOD5, new_data$BOD5)
  }
  
  Future = data.frame("BOD5" = as.numeric(BOD5), red_tide_predict)
  Futures = data.frame(Futures, Future$red_tide_predict)
}

predictions = Futures[-1]

rsquare_BOD5 = mean(rvalues)

# Calculate the means and confidence intervals for each row
BOD5mean_predictions <- rowMeans(predictions)
BOD5ci <- t(apply(predictions, 1, function(x) t.test(x, conf.int = TRUE)$conf.int))

# Create a data frame with the means, lower and upper CIs, and Years
BOD5data <- data.frame(Years, BOD5mean_predictions, lower_ci = BOD5ci[,1], upper_ci = BOD5ci[,2])

# Plot the means with confidence intervals
ggplot(BOD5data, aes(x = Years, y = BOD5mean_predictions)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.3) +
  xlab("Years") +
  ylab("Predictions")

Months = seq(1:337)/12
Years = seq(1:337)/12

rsquare_Temperature
rsquare_BOD5

# Create a combined dataframe for plotting
combined_data <- data.frame(
  Years = BOD5data$Years,
  BOD5mean_predictions = BOD5data$BOD5mean_predictions,
  lower_ci_BOD5 = BOD5data$lower_ci,
  upper_ci_BOD5 = BOD5data$upper_ci,
  Tempmean_predictions = Tempdata$Tempmean_predictions,
  lower_ci_Temp = Tempdata$lower_ci,
  upper_ci_Temp = Tempdata$upper_ci
)

combined_data$Years = combined_data$Years + 2022

# Plot the combined graph
TempBOD5 = ggplot(combined_data) +
  geom_line(aes(x = Years, y = BOD5mean_predictions, color = "BOD5"),show.legend = F) +
  geom_ribbon(aes(x = Years, ymin = lower_ci_BOD5, ymax = upper_ci_BOD5, fill = "BOD5"), alpha = 0.3, colour = "black") +
  geom_line(aes(x = Years, y = Tempmean_predictions, color = "Temprature"), show.legend = F) +
  geom_ribbon(aes(x = Years, ymin = lower_ci_Temp, ymax = upper_ci_Temp, fill = "Temperature"), alpha = 0.3, colour = "black") +
  xlab("Years") +
  labs(fill="Variable") +
  ylab("Predicted sightngs") +
  theme_minimal()
TempBOD5 

#####################################################################################

####################Predicting for all factors
new_data = store_new_data

new_prediction <- predict(trained_model, newdata = new_data)

cat("New Prediction:", new_prediction, "\n")

#Prediction for environment increase
red_tide_predict = c(new_prediction)
BOD5 = c(new_data$BOD5)
Orthophosphate_Phosphorus = c(new_data$Orthophosphate_Phosphorus)
pH = c(new_data$pH)
Salinity = c(new_data$Salinity)
Silica = c(new_data$Silica)
Suspended_solids = c(new_data$Suspended_solids)
Temperature = c(new_data$Temperature)
Total_phosphorus = c(new_data$Total_phosphorus)
Turbidity = c(new_data$Turbidity)
Volatile_suspended_solids = c(new_data$Volatile_suspended_solids)

for (i in 1:336){ #frm 2022 to 2050 at .01 degree Celcius per month (doesn't make sense, but just to check)
  
  new_data$BOD5 = new_data$BOD5 + as.vector(unlist(subset(slopes, Variable == "BOD5 (mg/L)")[3]/12)) 
  new_data$Orthophosphate_Phosphorus = new_data$Orthophosphate_Phosphorus + as.vector(unlist(subset(slopes, Variable == "Orthophosphate\nphosphorus (mg/L)")[3]/12)) 
  new_data$pH = new_data$pH + + as.vector(unlist(subset(slopes, Variable == "pH")[3]/12)) 
  new_data$Salinity = new_data$Salinity + as.vector(unlist(subset(slopes, Variable == "Salinity (psu)")[3]/12)) 
  new_data$Silica = new_data$Silica + as.vector(unlist(subset(slopes, Variable == "Silica (mg/L)")[3]/12)) 
  new_data$Suspended_solids = new_data$Suspended_solids + as.vector(unlist(subset(slopes, Variable == "Suspended\n solids (mg/L)")[3]/12)) 
  new_data$Temperature = new_data$Temperature + as.vector(unlist(subset(slopes, Variable == "Temperature (°C)")[3]/12)) 
  new_data$Total_phosphorus = new_data$Total_phosphorus + as.vector(unlist(subset(slopes, Variable == "Total\nphosphorus (mg/L)")[3]/12)) 
  new_data$Turbidity = new_data$Turbidity + as.vector(unlist(subset(slopes, Variable == "Turbidity (NTU)")[3]/12)) 
  new_data$Volatile_suspended_solids = new_data$Volatile_suspended_solids + as.vector(unlist(subset(slopes, Variable == "Volatile suspended\nsolids (mg/L)")[3]/12)) 
  
  new_prediction <- predict(trained_model, newdata = new_data)
  red_tide_predict = append(red_tide_predict, new_prediction)

}

Years = seq(1:length(red_tide_predict))/12

ggplot() +
  geom_line(aes(x = Years, y = red_tide_predict)) +
  labs(x = "Years", y = "Noctiluca scintillans Prediction") +
  theme_minimal() +
  ggtitle("Noctiluca scintillans Red Tide Sightings over time starting from average conditons in Tolo Harbour") +
  theme(plot.title = element_text(hjust = 0.5))

mean(as.numeric(unlist(modeldata[1])))
########

#Multiple models learning for the previous model
rvalues = c()
Futures = c(Years)

for (i in 1:100){
  set.seed(NULL)  # For reproducibility
  train_index <- createDataPartition(modeldata$Noctiluca_scintillans, p = 0.8, list = FALSE)
  train_data <- modeldata[train_index, ]
  test_data <- modeldata[-train_index, ]
  
  # Step 4: Model Selection
  # Choose a model for prediction (e.g., random forest)
  
  model <- randomForest(Noctiluca_scintillans ~ 
                          BOD5+
                          Orthophosphate_Phosphorus+
                          pH+
                          Salinity+
                          Silica+
                          Suspended_solids+
                          Temperature+
                          Total_phosphorus+
                          Turbidity+
                          Volatile_suspended_solids, 
                        data = train_data)
  
  
  # Step 5: Model Training
  # Train the model using the training dataset
  trained_model <- model
  
  # Step 6: Model Evaluation
  # Evaluate the model using the testing dataset
  predictions <- predict(trained_model, newdata = test_data)
  
  # Calculate evaluation metrics
  mae <- mean(abs(predictions - test_data$Noctiluca_scintillans))
  mse <- mean((predictions - test_data$Noctiluca_scintillans)^2)
  rmse <- sqrt(mse)
  r_squared <- 1 - sum((test_data$Noctiluca_scintillans - predictions)^2) / sum((test_data$Noctiluca_scintillans - mean(test_data$Noctiluca_scintillans))^2)
  
  rvalues = append(rvalues, r_squared) #store for later
  
  new_data = store_new_data
  
  new_prediction <- predict(trained_model, newdata = new_data)
  
  cat("New Prediction:", new_prediction, "\n")
  
  #Prediction for environment increase
  red_tide_predict = c(new_prediction)
  BOD5 = c(new_data$BOD5)
  Orthophosphate_Phosphorus = c(new_data$Orthophosphate_Phosphorus)
  pH = c(new_data$pH)
  Salinity = c(new_data$Salinity)
  Silica = c(new_data$Silica)
  Suspended_solids = c(new_data$Suspended_solids)
  Temperature = c(new_data$Temperature)
  Total_phosphorus = c(new_data$Total_phosphorus)
  Turbidity = c(new_data$Turbidity)
  Volatile_suspended_solids = c(new_data$Volatile_suspended_solids)
  
  for (i in 1:336){ #frm 2022 to 2050 at .01 degree Celcius per month (doesn't make sense, but just to check)
    
    new_data$BOD5 = new_data$BOD5 + as.vector(unlist(subset(slopes, Variable == "BOD5 (mg/L)")[3]/12)) 
    new_data$Orthophosphate_Phosphorus = new_data$Orthophosphate_Phosphorus + as.vector(unlist(subset(slopes, Variable == "Orthophosphate\nphosphorus (mg/L)")[3]/12)) 
    new_data$pH = new_data$pH + + as.vector(unlist(subset(slopes, Variable == "pH")[3]/12)) 
    new_data$Salinity = new_data$Salinity + as.vector(unlist(subset(slopes, Variable == "Salinity (psu)")[3]/12)) 
    new_data$Silica = new_data$Silica + as.vector(unlist(subset(slopes, Variable == "Silica (mg/L)")[3]/12)) 
    new_data$Suspended_solids = new_data$Suspended_solids + as.vector(unlist(subset(slopes, Variable == "Suspended\n solids (mg/L)")[3]/12)) 
    new_data$Temperature = new_data$Temperature + as.vector(unlist(subset(slopes, Variable == "Temperature (°C)")[3]/12)) 
    new_data$Total_phosphorus = new_data$Total_phosphorus + as.vector(unlist(subset(slopes, Variable == "Total\nphosphorus (mg/L)")[3]/12)) 
    new_data$Turbidity = new_data$Turbidity + as.vector(unlist(subset(slopes, Variable == "Turbidity (NTU)")[3]/12)) 
    new_data$Volatile_suspended_solids = new_data$Volatile_suspended_solids + as.vector(unlist(subset(slopes, Variable == "Volatile suspended\nsolids (mg/L)")[3]/12)) 
    
    new_prediction <- predict(trained_model, newdata = new_data)
    red_tide_predict = append(red_tide_predict, new_prediction)
    
  }
  
  Futures = data.frame(Futures, red_tide_predict)
}

rsquare = mean(rvalues)
Env_means = rowMeans(Futures[-1])
predictions = Futures[-1]

# Calculate the means and confidence intervals for each row
mean_predictions = rowMeans(predictions)
ci <- t(apply(predictions, 1, function(x) t.test(x, conf.int = TRUE)$conf.int))

Years = 2022+ Years
# Create a data frame with the means, lower and upper CIs, and Years
Alldata <- data.frame(Years, mean_predictions, lower_ci = ci[,1], upper_ci = ci[,2])

# Plot the means with confidence intervals
allplot = ggplot(Alldata, aes(x = Years, y = mean_predictions)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = "All variables"), alpha = 0.3, colour = "black") +
  xlab("Years") +
  scale_colour_manual(values = c("limegreen")) +
  scale_fill_manual(values = c("limegreen")) +
  labs(fill = NULL) +
  ylab("Predicted sightings per month") +
  theme_minimal()
allplot

grid.arrange(TempBOD5 + 
               ggtitle("A") +
               ylab("Predicted sightings per month") +
               theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(),
                     axis.title.y = element_text(size = 15),
                     axis.text.y = element_text(size = 15),
                     legend.text = element_text(size = 15),
                     legend.title = element_blank()),
             allplot +
               ylab("Predicted sightings per month") +
               ggtitle ("B") + 
               theme(axis.text = element_text(size = 15),
                     axis.title = element_text(size = 15),
                     legend.text = element_text(size = 15)), ncol = 1)

rsquare
rsquare_BOD5
rsquare_Temperature
