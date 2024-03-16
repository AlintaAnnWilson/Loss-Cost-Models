#install required libraries

install.packages("gbm")
install.packages("caret")
install.packages("Metrics")
install.packages("tweedie")
library(tweedie)
library(Metrics)
library(gbm)
library(caret)
library(dplyr)
#----------------------------------------tuned hyper params------------------------------------------------#



gbm_tuned <- gbm(
  ClaimNb ~   VehPower + VehAge + DrivAge + BonusMalus
  + VehBrand + VehGas + Region + Density + offset(log(Exposure)) , data = train, distribution = "poisson", n.trees = 515, 
  shrinkage = 0.01, interaction.depth = 7, n.minobsinnode = 5)



summary(gbmtuned)

#---GBM Freq model 2

gbmtuned_2 =gbm(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus
                + VehBrand + VehGas + Region + Area , data = train,
                distribution="poisson",n.trees= 515,shrinkage=0.01,interaction.depth=7,
                n.minobsinnode = 5 )

summary(gbmtuned_2)

#----GBM freq model 3

gbmtuned_3 =gbm(ClaimNb ~  VehPower + VehAge + DrivAge + BonusMalus
                + VehBrand + VehGas + Region + Density , data = train,
                distribution="poisson",n.trees=515,shrinkage=0.01,interaction.depth=7,
                n.minobsinnode = 5 )

summary(gbmtuned_3)


#Metrics Evaluation

# Predict on the test data
preds.gbm_1 <- predict(gbmtuned, newdata = test, n.trees =515, type = "response")
preds.gbm_2 <- predict(gbmtuned_2, newdata = test, n.trees = 515, type = "response")
preds.gbm_3 <- predict(gbmtuned_3, newdata = test, n.trees = 515, type = "response")

# Calculate Mean Absolute Error (MAE)
mae.gbm_1 <- mean(abs(test$ClaimNb - preds.gbm_1))
mae.gbm_2 <- mean(abs(test$ClaimNb - preds.gbm_2))
mae.gbm_3 <- mean(abs(test$ClaimNb - preds.gbm_3))


# Print the results
cat("MSE of model 1:", mae.gbm_1, "\n")
cat("MSE of model 2:", mae.gbm_2, "\n")
cat("MSE of model 3:", mae.gbm_3, "\n")





#--------------------------------severity--------------------------------------#

# Model 1 with all variables

gbmtuned.sev <- gbm(ClaimAmount ~  VehPower + VehAge + DrivAge + BonusMalus +
                      VehBrand + VehGas + Density + Region + Area + offset(log(ClaimNb)), data = train,
                    distribution = "laplace", n.trees = 278, shrinkage = 0.01,
                    interaction.depth = 5, n.minobsinnode = 5)

# Model 1 with all variables

gbmtuned.sev <- gbm(ClaimAmount ~  VehPower + VehAge + DrivAge + BonusMalus +
                      VehBrand + VehGas + Density + Region + Area + offset(log(ClaimNb)), data = train,
                    distribution = "laplace", n.trees = 278, shrinkage = 0.01,
                    interaction.depth = 5, n.minobsinnode = 5)

#calculating average claim cost from best severity model
pred_sev_GBM <- predict(gbmtuned.sev, newdata = test, n.trees = 278, type = "response")

fit_gamma <- fitdistr(pred_sev_GBM, densfun = "gamma")

  # Summary of the GBM Claim Severity model
  summary(gbmtuned.sev)

# Model 2 with only area
gbmtuned.sev2 <- gbm(ClaimAmount ~ VehPower + VehAge + DrivAge + BonusMalus +
                        VehBrand + VehGas + Region + Area, data = train,
                      distribution = "laplace", n.trees = 278, shrinkage = 0.01,
                      interaction.depth = 5, n.minobsinnode = 5)

# Summary of the GBM Claim Sev model 2
summary(gbmtuned.sev2)

# Model 3 with only density
gbmtuned.sev3 <- gbm(ClaimAmount ~ VehPower + VehAge + DrivAge + BonusMalus +
                        VehBrand + VehGas + Region + Density, data = train,
                      distribution = "laplace", n.trees = 278, shrinkage = 0.01,
                      interaction.depth = 5, n.minobsinnode = 5)

# Summary of the GBM Claim Sev model 3
summary(gbmtuned.sev3)


# Predict on the test data
preds.gbm.sev1 <- predict(gbmtuned.sev, newdata = test, n.trees = 278, type = "response")
preds.gbm.sev2 <- predict(gbmtuned.sev2, newdata = test, n.trees = 278, type = "response")
preds.gbm.sev3 <- predict(gbmtuned.sev3, newdata = test, n.trees = 278, type = "response")


# Calculate Mean Absolute Error (MAE)
mae.gbm.sev1 <- mean(abs(test$ClaimAmount- preds.gbm.sev1))
mae.gbm.sev2 <- mean(abs(test$ClaimAmount - preds.gbm.sev2))
mae.gbm.sev3 <- mean(abs(test$ClaimAmount - preds.gbm.sev3))


# Print the results
cat("MSE of model 1:", mae.gbm.sev1, "\n")
cat("MSE of model 2:", mae.gbm.sev2, "\n")
cat("MSE of model 3:", mae.gbm.sev3, "\n")


#---------------------------------------calculating loss cost-----------------------------------------------#

#Calculating expected claims from best freq model
pred_counts_GBM <- predict(gbm_tuned, newdata = test, n.trees = 515, type = "response")

#calculating average claim cost from best severity model
pred_sev_GBM <- predict(gbmtuned.sev, newdata = test, n.trees = 278, type = "response")


claim_amount_expected_GBM <- pred_sev_GBM * pred_counts_GBM

actual_claim_amount <- test$ClaimAmount * test$ClaimNb

# Calculate the loss for each observation
loss_cost_GBM <- abs(claim_amount_expected_GBM - actual_claim_amount)

# Calculate the mean loss (loss cost) for all observations
mean_loss_cost_GBM <- mean(loss_cost_GBM)



#----------------------------------------GBM before Capping-----------------------------------------------#




gbm_tuned_uncapped <- gbm(
  ClaimNb ~   VehPower + VehAge + DrivAge + BonusMalus
  + VehBrand + VehGas + Region + Density + offset(log(Exposure)) , data = train, distribution = "poisson", n.trees = 515, 
  shrinkage = 0.01, interaction.depth = 7, n.minobsinnode = 5)



summary(gbm_tuned_uncapped)



#-------------------------------------------------------severity--------------------------------------------#

# Model 1 with all variables

gbmtuned.sev_uncapped <- gbm(ClaimAmount ~  VehPower + VehAge + DrivAge + BonusMalus +
                      VehBrand + VehGas + Density + Region + Area + offset(log(ClaimNb)), data = train,
                    distribution = "laplace", n.trees = 278, shrinkage = 0.01,
                    interaction.depth = 5, n.minobsinnode = 5)

summary(gbmtuned.sev_uncapped)
#---------------------------------------calculating loss cost-----------------------------------------------#

#Calculating expected claims from best freq model
pred_counts_GBM_uncapped <- predict(gbm_tuned_uncapped, newdata = test, n.trees = 515, type = "response")

#calculating average claim cost from best severity model
pred_sev_GBM_uncapped <- predict(gbmtuned.sev_uncapped, newdata = test, n.trees = 278, type = "response")


claim_amount_expected_GBM_uncapped <- pred_sev_GBM_uncapped * pred_counts_GBM_uncapped

actual_claim_amount <- test$ClaimAmount * test$ClaimNb

# Calculate the loss for each observation
loss_cost_GBM_uncapped <- abs(claim_amount_expected_GBM_uncapped - actual_claim_amount_uncapped)

# Calculate the mean loss (loss cost) for all observations
mean_loss_cost_GBM_uncapped <- mean(loss_cost_GBM_uncapped)

