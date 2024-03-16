library(keras)
library(tensorflow)
library(rsample)

#----------------------------Pre processing for NN-----------------------------#
final_data_NN <- final_data

#Min-Max scaling for numerical features

# List of numerical features to be scaled 
numerical_features <- c("VehPower", "VehAge", "DrivAge", "BonusMalus", "Density")

# Min-Max Scaling function
min_max_scaling <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply Min-Max Scaling to numerical features
final_data_NN[numerical_features] <- lapply(final_data_NN[numerical_features], min_max_scaling)

# Print the scaled data frame
print(final_data_NN)

# List of categorical features to be one-hot encoded
categorical_features <- c("Area", "VehGas", "VehBrand", "Region")

# Function to perform one-hot encoding on categorical variables
one_hot_encode <- function(data, feature) {
  encoded_col <- factor(data[[feature]])
  levels_encoded <- levels(encoded_col)
  encoded_values <- as.integer(encoded_col)
  
  data[[paste0(feature, "_encoded")]] <- encoded_values
  data <- data[, !(names(data) %in% feature)]
  
  return(data)
}

# Apply one-hot encoding to each categorical feature
for (feature in categorical_features) {
  final_data_NN <- one_hot_encode(final_data_NN, feature)
}

# Print the modified data frame with one-hot encoded categorical features
print(final_data_NN)




# Split data into train and test sets after pre-processing
set.seed(123) 

data_split <- initial_split(final_data_NN, prop = 0.7) 
train_data <- training(data_split)
test_data <- testing(data_split)

# Verify the sizes of train and test sets
nrow(train_data)
nrow(test_data)

# Predictor variables
X_train <- train_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                          "VehBrand_encoded", "VehGas_encoded", "Region_encoded", "Area_encoded", "Density")]

X_test <- test_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                        "VehBrand_encoded", "VehGas_encoded", "Region_encoded", "Area_encoded", "Density")]

# Target variables
y_train <- train_data$ClaimNb
y_test <- test_data$ClaimNb





#---------------------------Model 1----------------------------------------#


set.seed(123)

model_ann_uncapped <- keras_model_sequential()
model_ann_uncapped %>%
  layer_dense(units = 64, activation = "tanh", input_shape = 9, name = "dense_input1")
model_ann_uncapped %>%
  layer_dense(units = 80, activation = "sigmoid")
model_ann_uncapped %>%
  layer_dense(units = 88, activation = "relu")
model_ann_uncapped %>%
  layer_dense(units = 1, activation = "linear")


# Define the optimizer
optimizer <- tf$keras$optimizers$legacy$Adam(learning_rate=0.01)

# Compile the model
model_ann_uncapped$compile(
  loss = "mean_squared_error",
  optimizer = optimizer
)


# Train the model and evaluate on validation data
freq_ann <- model_ann_uncapped %>% 
  fit(
    x = as.matrix(X_train),
    y = y_train,
    batch_size = 32,
    epochs = 50,
    verbose = 2,
    validation_split = 0.2
  )


# Evaluate the model on th test set
results_ann <- model_ann_uncapped %>% evaluate(
  x = as.matrix(X_test),
  y = y_test
)


# Make predictions on the test data
predictions_ann <- model_ann_uncapped %>% predict(as.matrix(X_test))

# Calculate the Mean Absolute Error (MAE)
mae_ann <- mean(abs(predictions_ann - y_test))

# Print the MAE
cat("Mean Absolute Error (MAE) of model 1:", mae_ann, "\n")



#-----------------------------Model2----------------------------------------#

# Predictor variables
X_train_2 <- train_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                            "VehBrand_encoded", "VehGas_encoded", 
                            "Region_encoded", "Area_encoded")]

X_test_2 <- test_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                          "VehBrand_encoded", "VehGas_encoded", 
                          "Region_encoded", "Area_encoded")]
# Target variables
y_train <- train_data$ClaimNb
y_test <- test_data$ClaimNb

model_ann_uncapped_2 <- keras_model_sequential()
model_ann_uncapped_2 %>%
  layer_dense(units = 64, activation = "tanh", input_shape = 8, name = "dense_input1")
model_ann_uncapped_2 %>%
  layer_dense(units = 80, activation = "sigmoid")
model_ann_uncapped_2 %>%
  layer_dense(units = 88, activation = "relu")
model_ann_uncapped_2 %>%
  layer_dense(units = 1, activation = "linear")


# Define the optimizer
optimizer <- tf$keras$optimizers$legacy$Adam(learning_rate=0.01)

# Compile the model
model_ann_uncapped_2$compile(
  loss = "mean_squared_error",
  optimizer = optimizer
)


# Train the model and evaluate on validation data
freq_ann_2 <- model_ann_uncapped_2 %>% 
  fit(
    x = as.matrix(X_train_2),
    y = y_train,
    batch_size = 32,
    epochs = 50,
    verbose = 2,
    validation_split = 0.2
  )


# Evaluate the model on the test set
results_ann2 <- model_ann_uncapped_2 %>% evaluate(
  x = as.matrix(X_test_2),
  y = y_test
)


# Make predictions on the test data
predictions_ann_2 <- model_ann_uncapped_2 %>% predict(as.matrix(X_test_2))

# Calculate the Mean Absolute Error (MAE)
mae_ann_2 <- mean(abs(predictions_ann_2 - y_test))

# Print the MAE
cat("Mean Absolute Error (MAE) of model 2:", mae_ann_2, "\n")



#---------------------------Model 3----------------------------------------#

# Predictor variables
X_train_3 <- train_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                            "VehBrand_encoded", "VehGas_encoded", 
                            "Region_encoded", "Density")]

X_test_3 <- test_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                          "VehBrand_encoded", "VehGas_encoded", 
                          "Region_encoded", "Density")]
# Target variables
y_train <- train_data$ClaimNb
y_test <- test_data$ClaimNb


model_ann_uncapped_3 <- keras_model_sequential()
model_ann_uncapped_3 %>%
  layer_dense(units = 64, activation = "tanh", input_shape = 8, name = "dense_input1")
model_ann_uncapped_3 %>%
  layer_dense(units = 80, activation = "sigmoid")
model_ann_uncapped_3 %>%
  layer_dense(units = 88, activation = "relu")
model_ann_uncapped_3 %>%
  layer_dense(units = 1, activation = "linear")


# Define the optimizer
optimizer <- tf$keras$optimizers$legacy$Nadam(learning_rate=0.01)

# Compile the model
model_ann_uncapped_3$compile(
  loss = "mean_squared_error",
  optimizer = optimizer
)


# Train the model and evaluate on validation data
freq_ann_3 <- model_ann_uncapped_3 %>% 
  fit(
    x = as.matrix(X_train_3),
    y = y_train,
    batch_size = 32,
    epochs = 50,
    verbose = 2,
    validation_split = 0.2
  )


# Evaluate the model on the test set
results_ann3 <- model_ann_uncapped_3 %>% evaluate(
  x = as.matrix(X_test_3),
  y = y_test
)


# Make predictions on the test data
predictions_ann_3 <- model_ann_uncapped_3 %>% predict(as.matrix(X_test_3))

# Calculate the Mean Absolute Error (MAE)
mae_ann_3 <- mean(abs(predictions_ann_3 - y_test))


# Print the MAE
cat("Mean Absolute Error (MAE) of model 1:", mae_ann, "\n")
cat("Mean Absolute Error (MAE) of model 1:", mae_ann_2, "\n")
cat("Mean Absolute Error (MAE) of model 3:", mae_ann_3, "\n")

#---------------------------Severity---------------------------------------#


# Predictor variables
X_train <- train_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                          "VehBrand_encoded", "VehGas_encoded", 
                          "Region_encoded", "Area_encoded", "Density")]

X_test <- test_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                        "VehBrand_encoded", "VehGas_encoded", 
                        "Region_encoded", "Area_encoded", "Density")]
# Target variables
y_train_sev <- train_data$ClaimAmount
y_test_sev<- test_data$ClaimAmount


model_ann_uncapped_sev <- keras_model_sequential()
model_ann_uncapped_sev %>%
  layer_dense(units = 256, activation = "sigmoid", input_shape = 9, name = "dense_input1")
model_ann_uncapped_sev %>%
  layer_dense(units = 32, activation = "tanh")
model_ann_uncapped_sev %>%
  layer_dense(units = 72, activation = "tanh")
model_ann_uncapped_sev %>%
  layer_dense(units = 88, activation = "tanh")
model_ann_uncapped_sev %>%
  layer_dense(units = 1, activation = "linear")


# Define the optimizer
optimizer <- tf$keras$optimizers$legacy$Adam(learning_rate=0.01)

# Compile the model
model_ann_uncapped_sev$compile(
  loss = "mean_absolute_error",
  optimizer = optimizer
)






# Train the model and evaluate on validation data
sev_ann <- model_ann_uncapped_sev %>% 
  fit(
    x = as.matrix(X_train),
    y = y_train_sev,
    batch_size = 32,
    epochs = 50,
    verbose = 2,
    validation_split = 0.2
  )


# Evaluate the model on the test set
test_resultssev <- model_ann_uncapped_sev %>% evaluate(
  x = as.matrix(X_test),
  y = y_test_sev
)


# Make predictions on the test data
predictions_ann_sev <- model_ann_uncapped_sev %>% predict(as.matrix(X_test))

# Calculate the Mean Absolute Error (MAE)
mae_ann_sev <- mean(abs(predictions_ann_sev - y_test_sev))

# Print the MAE
cat("Mean Absolute Error (MAE) of model 1:", mae_ann_sev, "\n")




#----------------------------Model 2--------------------------------------#


# Predictor variables
X_train_2 <- train_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                            "VehBrand_encoded", "VehGas_encoded", 
                            "Region_encoded", "Area_encoded")]

X_test_2 <- test_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                          "VehBrand_encoded", "VehGas_encoded", 
                          "Region_encoded", "Area_encoded")]
# Target variables
y_train_sev <- train_data$ClaimAmount
y_test_sev<- test_data$ClaimAmount

model_ann_uncapped_5 <- keras_model_sequential()
model_ann_uncapped_5 %>%
  layer_dense(units = 256, activation = "sigmoid", input_shape = 8, name = "dense_input1")
model_ann_uncapped_5 %>%
  layer_dense(units = 32, activation = "tanh")
model_ann_uncapped_5 %>%
  layer_dense(units = 72, activation = "tanh")
model_ann_uncapped_5 %>%
  layer_dense(units = 88, activation = "tanh")
model_ann_uncapped_5 %>%
  layer_dense(units = 1, activation = "linear")
# Define the optimizer
optimizer <- tf$keras$optimizers$legacy$Adam(learning_rate=0.01)


# Compile the model
model_ann_uncapped_5$compile(
  loss = "mean_absolute_error",
  optimizer = optimizer
)

# Train the model and evaluate on validation data
sev_ann_2 <- model_ann_uncapped_5 %>% 
  fit(
    x = as.matrix(X_train_2),
    y = y_train_sev,
    batch_size = 32,
    epochs = 50,
    verbose = 2,
    validation_split = 0.2
  )


# Evaluate the model on the test set
test_results2_sev <- model_ann_uncapped_5 %>% evaluate(
  x = as.matrix(X_test_2),
  y = y_test_sev
)


# Make predictions on the test data
predictions_ann_sev_2 <- model_ann_uncapped_5 %>% predict(as.matrix(X_test_2))

# Calculate the Mean Absolute Error (MAE)
mae_ann_sev_2 <- mean(abs(predictions_ann_sev_2 - y_test_sev))

# Print the MAE
cat("Mean Absolute Error (MAE) of model 2:", mae_ann_sev_2, "\n")




#----------------------------Model 3--------------------------------------#



# Predictor variables
X_train_3 <- train_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                            "VehBrand_encoded", "VehGas_encoded", 
                            "Region_encoded", "Density")]

X_test_3 <- test_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                          "VehBrand_encoded", "VehGas_encoded", 
                          "Region_encoded", "Density")]
# Target variables
y_train_sev <- train_data$ClaimAmount
y_test_sev<- test_data$ClaimAmount

model_ann_uncapped_6 <- keras_model_sequential()
model_ann_uncapped_6 %>%
  layer_dense(units = 256, activation = "sigmoid", input_shape = 8, name = "dense_input1")
model_ann_uncapped_6 %>%
  layer_dense(units = 32, activation = "tanh")
model_ann_uncapped_6 %>%
  layer_dense(units = 72, activation = "tanh")
model_ann_uncapped_6 %>%
  layer_dense(units = 88, activation = "tanh")
model_ann_uncapped_6 %>%
  layer_dense(units = 1, activation = "linear")

# Define the optimizer
optimizer <- tf$keras$optimizers$legacy$Nadam(learning_rate=0.01)


# Compile the model
model_ann_uncapped_6$compile(
  loss = "mean_absolute_error",
  optimizer = optimizer
)


# Train the model and evaluate on validation data
sev_ann_3 <- model_ann_uncapped_6 %>% 
  fit(
    x = as.matrix(X_train_3),
    y = y_train_sev,
    batch_size = 32,
    epochs = 50,
    verbose = 2,
    validation_split = 0.2
  )


# Evaluate the model on the test set
test_results3_sev <- model %>% evaluate(
  x = as.matrix(X_test_3),
  y = y_test_sev
)


# Make predictions on the test data
predictions_ann_sev_3 <- model_ann_uncapped_6 %>% predict(as.matrix(X_test_3))

# Calculate the Mean Absolute Error (MAE)
mae_ann_sev_3 <- mean(abs(predictions_ann_sev_3 - y_test_sev))

# Print the MAE
cat("Mean Absolute Error (MAE) of model 3:", mae_ann_sev_3, "\n")



# Print the MAE
cat("Mean Absolute Error (MAE) of model 1:", mae_ann_sev, "\n")
cat("Mean Absolute Error (MAE) of model 2:", mae_ann_sev_2, "\n")
cat("Mean Absolute Error (MAE) of model 3:", mae_ann_sev_3, "\n")




#------------------calculating loss cost-------------------------------#

#Calculating expected claims from best freq model
predictions_ann <- model_ann_uncapped %>% predict(as.matrix(X_test))

#Calculating expected costs from best sev model
predictions_ann_sev <- model_ann_uncapped_sev %>% predict(as.matrix(X_test))

claim_amount_expected_ANN <- predictions_ann * predictions_ann_sev


actual_claim_amount_NN <- test_data$ClaimAmount * test_data$ClaimNb 

# Calculate the loss for each observation
loss_cost_ANN <- abs(claim_amount_expected_ANN - actual_claim_amount_NN)

# Calculate the mean loss (loss cost) for all observations
mean_loss_cost_ANN <- mean(loss_cost_ANN)

#----------------------------------------Modelling uncapped----------------------------------------------#

#---------------------------------------Pre processing for NN------------------------------------------#


merged_data_uncapped_NN <- merged_data

merged_data_uncapped_NN<-unique(merged_data_uncapped_NN)

nrow(merged_data_uncapped_NN)

merged_data_uncapped_NN <- subset(merged_data_uncapped_NN, ClaimAmount != 0)


# Set the threshold as the 99.9th percentile
threshold_uncapped_NN <- quantile(merged_data_uncapped_NN$ClaimAmount, 0.9999)

# Replace values above the threshold with the threshold value
merged_data_uncapped_NN$ClaimAmount <- pmin(merged_data_uncapped_NN$ClaimAmount, threshold)

# Check the summary statistics after capping extreme values
summary(merged_data_uncapped_NN$ClaimAmount)

#Min-Max scaling for numerical features

# List of numerical features to be scaled 
numerical_features <- c("VehPower", "VehAge", "DrivAge", "BonusMalus", "Density")

# Min-Max Scaling function
min_max_scaling <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply Min-Max Scaling to numerical features
merged_data_uncapped_NN[numerical_features] <- lapply(merged_data_uncapped_NN[numerical_features], min_max_scaling)

# Print the scaled data frame
print(merged_data_uncapped_NN)

# List of categorical features to be one-hot encoded
categorical_features <- c("Area", "VehGas", "VehBrand", "Region")

# Function to perform one-hot encoding on categorical variables
one_hot_encode <- function(data, feature) {
  encoded_col <- factor(data[[feature]])
  levels_encoded <- levels(encoded_col)
  encoded_values <- as.integer(encoded_col)
  
  data[[paste0(feature, "_encoded")]] <- encoded_values
  data <- data[, !(names(data) %in% feature)]
  
  return(data)
}

# Apply one-hot encoding to each categorical feature
for (feature in categorical_features) {
  merged_data_uncapped_NN <- one_hot_encode(merged_data_uncapped_NN, feature)
}

# Print the modified data frame with one-hot encoded categorical features
print(merged_data_uncapped_NN)




# Split data into train and test sets after pre-processing
set.seed(123) 

data_split <- initial_split(merged_data_uncapped_NN, prop = 0.7) 
train_data <- training(data_split)
test_data <- testing(data_split)

# Verify the sizes of train and test sets
nrow(train_data)
nrow(test_data)

# Predictor variables
X_train <- train_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                          "VehBrand_encoded", "VehGas_encoded", "Region_encoded", "Area_encoded", "Density")]

X_test <- test_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                        "VehBrand_encoded", "VehGas_encoded", "Region_encoded", "Area_encoded", "Density")]

# Target variables
y_train <- train_data$ClaimNb
y_test <- test_data$ClaimNb





#---------------------------Model 1----------------------------------------#


set.seed(123)

model_ann_uncapped <- keras_model_sequential()
model_ann_uncapped %>%
  layer_dense(units = 64, activation = "tanh", input_shape = 9, name = "dense_input1")
model_ann_uncapped %>%
  layer_dense(units = 80, activation = "sigmoid")
model_ann_uncapped %>%
  layer_dense(units = 88, activation = "relu")
model_ann_uncapped %>%
  layer_dense(units = 1, activation = "linear")


# Define the optimizer
optimizer <- tf$keras$optimizers$legacy$Adam(learning_rate=0.01)

# Compile the model
model_ann_uncapped$compile(
  loss = "mean_squared_error",
  optimizer = optimizer
)


# Train the model and evaluate on validation data
freq_ann_uncapped <- model_ann_uncapped %>% 
  fit(
    x = as.matrix(X_train),
    y = y_train,
    batch_size = 32,
    epochs = 50,
    verbose = 2,
    validation_split = 0.2
  )


# Evaluate the model on th test set
results_ann_uncapped <- model_ann_uncapped %>% evaluate(
  x = as.matrix(X_test),
  y = y_test
)


# Make predictions on the test data
predictions_ann_uncapped <- model_ann_uncapped %>% predict(as.matrix(X_test))



#---------------------------Severity---------------------------------------#


# Predictor variables
X_train <- train_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                          "VehBrand_encoded", "VehGas_encoded", 
                          "Region_encoded", "Area_encoded", "Density")]

X_test <- test_data[, c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                        "VehBrand_encoded", "VehGas_encoded", 
                        "Region_encoded", "Area_encoded", "Density")]
# Target variables
y_train_sev <- train_data$ClaimAmount
y_test_sev<- test_data$ClaimAmount


model_ann_uncapped_sev <- keras_model_sequential()
model_ann_uncapped_sev %>%
  layer_dense(units = 256, activation = "sigmoid", input_shape = 9, name = "dense_input1")
model_ann_uncapped_sev %>%
  layer_dense(units = 32, activation = "tanh")
model_ann_uncapped_sev %>%
  layer_dense(units = 72, activation = "tanh")
model_ann_uncapped_sev %>%
  layer_dense(units = 88, activation = "tanh")
model_ann_uncapped_sev %>%
  layer_dense(units = 1, activation = "linear")


# Define the optimizer
optimizer <- tf$keras$optimizers$legacy$Adam(learning_rate=0.01)

# Compile the model
model_ann_uncapped_sev$compile(
  loss = "mean_absolute_error",
  optimizer = optimizer
)


# Train the model and evaluate on validation data
sev_ann_uncapped <- model_ann_uncapped_sev %>% 
  fit(
    x = as.matrix(X_train),
    y = y_train_sev,
    batch_size = 32,
    epochs = 50,
    verbose = 2,
    validation_split = 0.2
  )


# Evaluate the model on the test set
test_resultssev <- model_ann_uncapped_sev %>% evaluate(
  x = as.matrix(X_test),
  y = y_test_sev
)


# Make predictions on the test data
predictions_ann_sev_uncapped <- model_ann_uncapped_sev %>% predict(as.matrix(X_test))


#------------------calculating loss cost-------------------------------#

#Calculating expected claims from best freq model
predictions_ann_uncapped <- model_ann_uncapped %>% predict(as.matrix(X_test))

#Calculating expected costs from best sev model
predictions_ann_sev_uncapped <- model_ann_uncapped_sev %>% predict(as.matrix(X_test))

claim_amount_expected_ANN_uncapped <- predictions_ann_uncapped * predictions_ann_sev_uncapped


actual_claim_amount_NN <- test_data$ClaimAmount * test_data$ClaimNb 

# Calculate the loss for each observation
loss_cost_ANN_uncapped <- abs(claim_amount_expected_ANN_uncapped - actual_claim_amount_NN)

# Calculate the mean loss (loss cost) for all observations
mean_loss_cost_ANN_uncapped <- mean(loss_cost_ANN_uncapped)

