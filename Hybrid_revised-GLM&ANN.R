
#--------------------------------------------Hybrid Model--------------------------------------------------------#

#Combined best frequency models from GLM and NN
neg_binom_glm <- glm.nb(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus
                                    + VehBrand + VehGas + Region + Area+ Density + offset(log(Exposure)), 
                                    data = train)

predicted_counts <- predict(neg_binom_glm, newdata = train, type = "response")
pred_counts_GLM <- predict(neg_binom_glm, newdata = test, type = "response")


#Defining Optimizer
optimizer <- tf$keras$optimizers$legacy$Adam(learning_rate=0.01)

# Build your two input layers
input1 <- layer_input(shape = c(9), name = "input1")  
input2 <- layer_input(shape = c(1), name = "input2")  


dense1 <- input1 %>%
  layer_dense(units = 64, activation = "tanh")
dense2 <- dense1 %>%
  layer_dense(units = 80, activation = "sigmoid")
dense3 <- dense2 %>%
  layer_dense(units = 88, activation = "relu")


# Merge the neural network layers with the additional input
merged <- layer_concatenate(inputs = list(dense3, input2))

# Final output layer
output <- merged %>%
  layer_dense(units = 1, activation = "linear")

# Create the model
hybrid_model <- keras_model(inputs = list(input1, input2), outputs = output)

# Compile the model
hybrid_model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer
)

# Train the model and evaluate on validation data
freq_hybrid <- hybrid_model %>% 
  fit(
    x = list(as.matrix(X_train), predicted_counts), 
    y = y_train,
    batch_size = 32,
    epochs = 50,
    verbose = 2,
    validation_split = 0.2
  )

predictions_hybrid <- hybrid_model %>% predict(list(as.matrix(X_test), pred_counts_GLM))




#----------------------------Combining best severity models from GLM and NN--------------------------------------#

gamma_model <- glm(ClaimAmount ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region + Area,
                   data = train, family = Gamma(link = "log"),
                   offset = log(ClaimNb))

pred_gamma_train <- predict(gamma_model, newdata = train, type = "response")
pred_sev_GLM <- predict(gamma_model, newdata = test, type = "response")


# Build your two input layers
input1 <- layer_input(shape = c(9), name = "input1")  
input2 <- layer_input(shape = c(1), name = "input2")  


dense1 <- input1 %>%
  layer_dense(units = 256, activation = "sigmoid")
dense2 <- dense1 %>%
  layer_dense(units = 32, activation = "tanh")
dense3 <- dense2 %>%
  layer_dense(units = 72, activation = "tanh")
dense4 <- dense3 %>%
  layer_dense(units = 88, activation = "tanh")


# Merge the neural network layers with the additional input
merged <- layer_concatenate(inputs = list(dense4, input2))

# Final output layer
output <- merged %>%
  layer_dense(units = 1, activation = "linear")

# Create the model
hybrid_model_2 <- keras_model(inputs = list(input1, input2), outputs = output)

# Compile the model
hybrid_model_2 %>% compile(
  loss = "mean_absolute_error",
  optimizer = optimizer
)


# Train the model and evaluate on validation data
sev_hybrid <- hybrid_model_2 %>% 
  fit(
    x = list(as.matrix(X_train), pred_gamma_train),  
    y = y_train_sev,
    batch_size = 32,
    epochs = 50,
    verbose = 2,
    validation_split = 0.2
  )

# Make predictions on the test data
predictions_hybrid.sev <- hybrid_model_2 %>% predict(list(as.matrix(X_test), pred_sev_GLM))



#-----------------------------------Calculating loss cost---------------------------#

predictions_hybrid <- hybrid_model %>% predict(list(as.matrix(X_test), pred_counts_GLM))

predictions_hybrid.sev <- hybrid_model_2 %>% predict(list(as.matrix(X_test), pred_sev_GLM))


claim_amount_expected_hybrid <- predictions_hybrid * predictions_hybrid.sev

actual_claim_amount_NN <- test_data$ClaimAmount * test_data$ClaimNb

# Calculate the loss for each observation
loss_cost_hybrid <- abs(claim_amount_expected_hybrid - actual_claim_amount_NN)

# Calculate the mean loss (loss cost) for all observations
mean_loss_cost_hybrid <- mean(loss_cost_hybrid)




#----------------------------------Models before Capping-------------------------------------------------#


#Combined best frequency models from GLM and NN
neg_binom_glm_uncapped <- glm.nb(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus
                                    + VehBrand + VehGas + Region + Area+ Density + offset(log(Exposure)), 
                                    data = train)

predicted_counts_uncapped <- predict(neg_binom_glm_uncapped, newdata = train, type = "response")
pred_counts_GLM_uncapped <- predict(neg_binom_glm_uncapped, newdata = test, type = "response")


#Defining Optimizer
optimizer <- tf$keras$optimizers$legacy$Adam(learning_rate=0.01)

# Build your two input layers
input1 <- layer_input(shape = c(9), name = "input1")  
input2 <- layer_input(shape = c(1), name = "input2")  


dense1 <- input1 %>%
  layer_dense(units = 64, activation = "tanh")
dense2 <- dense1 %>%
  layer_dense(units = 80, activation = "sigmoid")
dense3 <- dense2 %>%
  layer_dense(units = 88, activation = "relu")


# Merge the neural network layers with the additional input
merged <- layer_concatenate(inputs = list(dense3, input2))

# Final output layer
output <- merged %>%
  layer_dense(units = 1, activation = "linear")

# Create the model
hybrid_model_uncapped <- keras_model(inputs = list(input1, input2), outputs = output)

# Compile the model
hybrid_model_uncapped %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer
)

# Train the model and evaluate on validation data
freq_hybrid_uncapped <- hybrid_model_uncapped %>% 
  fit(
    x = list(as.matrix(X_train), predicted_counts_uncapped), 
    y = y_train,
    batch_size = 32,
    epochs = 50,
    verbose = 2,
    validation_split = 0.2
  )

predictions_hybrid_uncapped <- hybrid_model_uncapped %>% predict(list(as.matrix(X_test), pred_counts_GLM_uncapped))




#-------------------------Combining best severity models from GLM and NN-----------------#

gamma_model_uncapped <- glm(ClaimAmount ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region + Area,
                   data = train, family = Gamma(link = "log"),
                   offset = log(ClaimNb))

pred_gamma_train_uncapped <- predict(gamma_model_uncapped, newdata = train, type = "response")
pred_sev_GLM_uncapped <- predict(gamma_model_uncapped, newdata = test, type = "response")


# Build your two input layers
input1 <- layer_input(shape = c(9), name = "input1")  
input2 <- layer_input(shape = c(1), name = "input2")  


dense1 <- input1 %>%
  layer_dense(units = 256, activation = "sigmoid")
dense2 <- dense1 %>%
  layer_dense(units = 32, activation = "tanh")
dense3 <- dense2 %>%
  layer_dense(units = 72, activation = "tanh")
dense4 <- dense3 %>%
  layer_dense(units = 88, activation = "tanh")


# Merge the neural network layers with the additional input
merged <- layer_concatenate(inputs = list(dense4, input2))

# Final output layer
output <- merged %>%
  layer_dense(units = 1, activation = "linear")

# Create the model
hybrid_model_sev_uncapped <- keras_model(inputs = list(input1, input2), outputs = output)

# Compile the model
hybrid_model_sev_uncapped %>% compile(
  loss = "mean_absolute_error",
  optimizer = optimizer
)


# Train the model and evaluate on validation data
sev_hybrid_uncapped <- hybrid_model_sev_uncapped %>% 
  fit(
    x = list(as.matrix(X_train), pred_gamma_train_uncapped),  
    y = y_train_sev,
    batch_size = 32,
    epochs = 50,
    verbose = 2,
    validation_split = 0.2
  )

# Make predictions on the test data
predictions_hybrid.sev_uncapped <- hybrid_model_sev_uncapped %>% predict(list(as.matrix(X_test), pred_sev_GLM_uncapped))



#-----------------------------------Calculating loss cost---------------------------#

predictions_hybrid_uncapped <- hybrid_model_uncapped %>% predict(list(as.matrix(X_test), pred_counts_GLM_uncapped))

predictions_hybrid.sev_uncapped <- hybrid_model_sev_uncapped %>% predict(list(as.matrix(X_test), pred_sev_GLM_uncapped))


claim_amount_expected_hybrid_uncapped <- predictions_hybrid_uncapped * predictions_hybrid.sev_uncapped

actual_claim_amount_NN <- test_data$ClaimAmount * test_data$ClaimNb

# Calculate the loss for each observation
loss_cost_hybrid_uncapped <- abs(claim_amount_expected_hybrid_uncapped - actual_claim_amount_NN)

# Calculate the mean loss (loss cost) for all observations
mean_loss_cost_hybrid_uncapped <- mean(loss_cost_hybrid_uncapped)






