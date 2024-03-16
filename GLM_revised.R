install.packages("reshape2")
install.packages("ggplot2")
install.packages("magrittr")
install.packages("dplyr")
install.packages("e1071")
install.packages("AER")
install.packages("MASS")
install.packages("countreg", repos="http://R-Forge.R-project.org")
install.packages("boot")
install.packages("caret")
install.packages("rpart")
install.packages("caTools")
install.packages("ISLR")
install.packages("pscl")
install.packages("glmmTMB")
install.packages('TMB', type = 'source')
install.packages("lmerTest") #anova models comparison glmmTB
install.packages("rsample")
install.packages("ineq")
install.packages("lmtest")
library(lmtest)
library(ineq)
library(lmerTest)
library(glmmTMB)
library(ISLR)
library(pscl)
library(caTools)
library(caret)
library(rpart)
library(boot)
library(countreg)
library(MASS)
library(AER)
library(e1071)
library(dplyr) #groupby
library(reshape2)
library(ggplot2)
library(magrittr) #> pipe
library(rsample)
library(gbm)


#Downloading the dataset

install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
library(CASdatasets)
?CASdatasets
data(freMTPL2freq)
data(freMTPL2sev)
write.csv(freMTPL2freq, file = "freMTPL2freq.csv")
write.csv(freMTPL2sev, file = "freMTPL2sev.csv")
#-----------------------------------------frequency data-----------------------------------------------------------------------#

freq_data <- read.csv("/Users/ronypabraham/Downloads/freMTPL2freq.csv")

#--------------------------------------------severity data---------------------------------------------------------------------#

sev_data <- read.csv("/Users/ronypabraham/Desktop/freMTPL2sev.csv")

str(freq_data)
str(sev_data)


#merging both data frames
merged_data <- merge(freq_data, sev_data, by = "IDpol", all.x =  TRUE)

#writing the file
#write.csv(merged_data, file = "merged_final.csv", row.names = FALSE)

#Correcting na values to 0

# Remove rows where ClaimNb > 0 and ClaimAmount is NA
merged_data <- merged_data[!(merged_data$ClaimNb > 0 & is.na(merged_data$ClaimAmount)), ]

# Convert NA in ClaimAmount to 0 when ClaimAmount = 0 
merged_data$ClaimAmount[is.na(merged_data$ClaimAmount)] <- 0

#removing any other rows
merged_data[is.na(merged_data)] <- 0

#missing values 
sapply(merged_data, function(x) {sum(is.na(x))})

#null values
is.null(merged_data)


# Conerting to factor

merged_data$Area <- as.numeric(as.factor(merged_data$Area))
merged_data$VehBrand <- as.factor(merged_data$VehBrand)
merged_data$VehGas <- as.numeric(as.factor(merged_data$VehGas))
merged_data$Region <- as.factor(merged_data$Region)
merged_data$VehPower <- as.numeric(merged_data$VehPower)
merged_data$BonusMalus <- as.integer(merged_data$BonusMalus)
merged_data$Density <- as.numeric(merged_data$Density)
merged_data$ClaimNb <- as.double(merged_data$ClaimNb)

#Dataframe "merged_data" is used for plotting distributions before capping the features and its given in the file "EDA"

#-------------------------------------------------outliers----------------------------------------------------------#
filtered_data <-merged_data

#Correcting exposures greater than 1 to 1

filtered_data$Exposure[filtered_data$Exposure > 1] <- 1

#capping vehicle age at 20

filtered_data$VehAge <- ifelse(filtered_data$VehAge > 20, 20, 
                               filtered_data$VehAge)

#capping driver age at 90

filtered_data$DrivAge <- pmin(filtered_data$DrivAge, 90)

#capping bonusmalus at 150

filtered_data$BonusMalus <- pmin(filtered_data$BonusMalus, 150)

#converting density to log scale

filtered_data$Density <- log(filtered_data$Density)


#Capping VehPower at 13
filtered_data$VehPower <- pmin(filtered_data$VehPower, 13)

#Capping ClaimNb at 4
filtered_data$ClaimNb <- pmin(filtered_data$ClaimNb,4)

#Dataframe "filtered_data" is used for plotting distributions before capping the features and its given in the file "EDA"

#Remove duplicate rows

final_data<-unique(filtered_data)

nrow(final_data)

#remove rows where claim amount is zero.
final_data <- subset(final_data, ClaimAmount != 0)


# Set the threshold as the 99.9th percentile
threshold <- quantile(final_data$ClaimAmount, 0.9999)

# Replace values above the threshold with the threshold value
final_data$ClaimAmount <- pmin(final_data$ClaimAmount, threshold)

# Check the summary statistics after capping extreme values
summary(final_data$ClaimAmount)


#-----------------------------------------GLM---------------------------------------------------------#




set.seed(123)


ind <- initial_split(final_data, prop = 0.7)  # 70% for training, 30% for testing
train <- training(ind)
test <- testing(ind)
cat("The number of training set is ", nrow(train) ,"\n")
cat("The number of test set is", nrow(test))



#---------------------------------GLM model 1 with Area & Density----------------------------------------

#Build Poisson GLM

poissonglm <- glm(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Region + Area+ Density +
                  offset(log(Exposure)), data = train,
                  family = "poisson")

glm.nb()

summary(poissonglm)

#Build Negative Binomial GLM
neg_binom_glm <- glm.nb(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus
                            + VehBrand + VehGas + Region + Area+ Density + offset(log(Exposure)), 
                            data = train)
summary(neg_binom_glm)


#Compare their deviances
deviance(poissonglm)
deviance(neg_binom_glm)


#---GLM Model 2--With only area

neg_binom_glm2 <- nb_model <- glm.nb(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus
                                    + VehBrand + VehGas + Region + Area + offset(log(Exposure)), 
                                    data = train)



#---GLM Model 3--With only density

neg_binom_glm3 <- nb_model <- glm.nb(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus
                                    + VehBrand + VehGas + Region + Density + offset(log(Exposure)), 
                                    data = train)



#-------K-Cross Validation---------------------

actual_counts <- test$ClaimNb
actual_counts2 <- train$ClaimNb

# Set the value of k (number of folds) for cross-validation
k <- 10

# k-fold cross-validation on test data
cv_test_1 <- cv.glm(data = test,glmfit = neg_binom_glm, K = k)
cv_test_2 <- cv.glm(data = test,glmfit = neg_binom_glm2, K = k)
cv_test_3 <- cv.glm(data = test,glmfit = neg_binom_glm3, K = k)


cv_dev_1 <- cv_test_1$delta
cv_dev_2 <- cv_test_2$delta
cv_dev_3 <- cv_test_3$delta


cv_pred_1 <- fitted(neg_binom_glm) - cv_dev_1
cv_pred_2 <- fitted(neg_binom_glm2) - cv_dev_2
cv_pred_3 <- fitted(neg_binom_glm3) - cv_dev_3

mae_cv_1 <- mean(abs(actual_counts - cv_pred_1))
mae_cv_2 <- mean(abs(actual_counts - cv_pred_2))
mae_cv_3 <- mean(abs(actual_counts - cv_pred_3))


cat("MAE - With all response variables: ", mae_cv_1, "\n")
cat("MAE- With only Area : ", mae_cv_2, "\n")
cat("MAE- With only Density: ", mae_cv_3, "\n")


#-----------------------K cross on train data-----------------------

# Perform k-fold cross-validation
cv_train_1 <- cv.glm(data = train,glmfit = neg_binom_glm, K = k)
cv_train_2 <- cv.glm(data = train,glmfit = neg_binom_glm2, K = k)
cv_train_3 <- cv.glm(data = train,glmfit = neg_binom_glm3, K = k)


cv_dev_tr_1 <- cv_train_1$delta
cv_dev_tr_2 <- cv_train_2$delta
cv_dev_tr_3 <- cv_train_3$delta


cv_pred_tr_1 <- fitted(neg_binom_glm) - cv_dev_tr_1
cv_pred_tr_2 <- fitted(neg_binom_glm2) - cv_dev_tr_2
cv_pred_tr_3 <- fitted(neg_binom_glm3) - cv_dev_tr_3


mae_cv_tr_1 <- mean(abs(actual_counts2 - cv_pred_tr_1))
mae_cv_tr_2 <- mean(abs(actual_counts2 - cv_pred_tr_2))
mae_cv_tr_3 <- mean(abs(actual_counts2 - cv_pred_tr_3))


cat("MAE - With all response variables: ", mae_cv_tr_1, "\n")
cat("MAE- With only Area: ", mae_cv_tr_2, "\n")
cat("MAE- With only density: ", mae_cv_tr_3, "\n")



# Kcross test data prediction 
model_names <- c("Model 1", "With Area", "With Density")
mae_values3 <- c(mae_cv_1, mae_cv_2, mae_cv_3)

mae_data3 <- data.frame(Model = model_names, MAE = mae_values3)

# Create the bar plot
ggplot(mae_data3, aes(x = Model, y = MAE)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Mean Absolute Error (MAE) for Different Models",
       x = "Model",
       y = "Mean Absolute Error (MAE)") +
  theme_minimal()





#---------------------------------------------------Claim Severity ------------------------------------------------------------#


#Build Gamma model
gamma_model <- glm(ClaimAmount ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region + Area,
                   data = train, family = Gamma(link = "log"),
                   offset = log(ClaimNb))

#build Log Normal model

log_normal_model <- glm(log(ClaimAmount) ~ VehPower + VehAge + DrivAge + BonusMalus +
                          VehBrand + VehGas + Density + Region + Area,
                   data = train, family = gaussian(link = "log"),
                   offset = log(ClaimNb))


summary(gamma_model)
summary(log_normal_model)

#Compare their deviances
deviance(gamma_model)
deviance(log_normal_model)

pred_gamma_train <- predict(gamma_model, newdata = train, type = "response")

#------------------------------Model 2 ----With Area


gamma_model2 <- glm(ClaimAmount ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Region + Area,
                   data = train, family = Gamma(link = "log"),
                   offset = log(ClaimNb))


#-------------------------Model 3----With Density

gamma_model3 <- glm(ClaimAmount ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region,
                    data = train, family = Gamma(link = "log"),
                    offset = log(ClaimNb))


#-------K-Cross Validation---------------------


# Set the value of k (number of folds) for cross-validation
k <- 10

# k-fold cross-validation on test data
cv_test_g_1 <- cv.glm(data = test,glmfit = gamma_model, K = k)
cv_test_g_2 <- cv.glm(data = test,glmfit = gamma_model2, K = k)
cv_test_g_3 <- cv.glm(data = test,glmfit = gamma_model3, K = k)


cv_dev_g_1 <- cv_test_g_1$delta
cv_dev_g_2 <- cv_test_g_2$delta
cv_dev_g_3 <- cv_test_g_3$delta


cv_pred_g_1 <- fitted(gamma_model) - cv_dev_g_1
cv_pred_g_2 <- fitted(gamma_model2) - cv_dev_g_2
cv_pred_g_3 <- fitted(gamma_model3) - cv_dev_g_3

mae_cv_g_1 <- mean(abs(actual_counts - cv_pred_g_1))
mae_cv_g_2 <- mean(abs(actual_counts - cv_pred_g_2))
mae_cv_g_3 <- mean(abs(actual_counts - cv_pred_g_3))


cat("MAE - With all response variables: ", mae_cv_g_1, "\n")
cat("MAE- With only Area : ", mae_cv_g_2, "\n")
cat("MAE- With only Density: ", mae_cv_g_3, "\n")


#-----------------------K cross on train data-----------------------

# Set the value of k (number of folds) for cross-validation
k <- 10

# k-fold cross-validation on test data
cv_train_g_1 <- cv.glm(data = train,glmfit = gamma_model, K = k)
cv_train_g_2 <- cv.glm(data = train,glmfit = gamma_model2, K = k)
cv_train_g_3 <- cv.glm(data = train,glmfit = gamma_model3, K = k)


cv_dev_g_1_tr <- cv_train_g_1$delta
cv_dev_g_2_tr <- cv_train_g_2$delta
cv_dev_g_3_tr <- cv_train_g_3$delta


cv_pred_g_1_tr <- fitted(gamma_model) - cv_dev_g_1_tr
cv_pred_g_2_tr <- fitted(gamma_model2) - cv_dev_g_2_tr
cv_pred_g_3_tr <- fitted(gamma_model3) - cv_dev_g_3_tr

mae_cv_g_1_tr <- mean(abs(actual_counts2 - cv_pred_g_1))
mae_cv_g_2_tr <- mean(abs(actual_counts2 - cv_pred_g_2))
mae_cv_g_3_tr <- mean(abs(actual_counts2 - cv_pred_g_3))


cat("MAE - With all response variables: ", mae_cv_g_1_tr, "\n")
cat("MAE- With only Area : ", mae_cv_g_2_tr, "\n")
cat("MAE- With only Density: ", mae_cv_g_3_tr, "\n")






#------------------calculating loss cost-------------------------------#

#Calculating expected claims from best freq model
pred_counts_GLM <- predict(neg_binom_glm, newdata = test, type = "response")

#calculating average claim cost from best severity model
pred_sev_GLM <- predict(gamma_model, newdata = test, type = "response")

#calculate expected claim amount by multiplying predictions from freq and sev model
claim_amount_expected_GLM <- pred_sev_GLM * pred_counts_GLM

#calculate actual claim amount
actual_claim_amount <- test$ClaimAmount * test$ClaimNb

# Calculate the loss for each observation
loss_cost_GLM <- abs(claim_amount_expected_GLM - actual_claim_amount)


# Calculate the mean loss (loss cost) for all observations
mean_loss_cost_GLM <- mean(loss_cost_GLM)

rmse(actual_claim_amount,claim_amount_expected_GLM)


#----------------------------MAE final plot------------------------------#

par(mfrow=c(1,2))

# Load necessary libraries
library(ggplot2)

# Create a data frame with model names and corresponding MAE values

mae_loss_cost <- data.frame(Model = c("GLM", "GBM", "ANN", "Hybrid Model"),
                            MAE = c(mean_loss_cost_GLM, mean_loss_cost_GBM, mean_loss_cost_ANN, mean_loss_cost_hybrid))

# Define custom colors
custom_colors <- c("blue", "green", "red", "orange") # Add more colors if needed

# Plot MAE values
ggplot(mae_loss_cost, aes(x = Model, y = MAE, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of Mean Absolute Error (MAE) for Different Models",
       x = "Models", y = "Mean Absolute Error (MAE)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = custom_colors)




#-----------------------------------AVE------------------------------------------------------#


# Sort the filtered actual claim amount in ascending order
sorted_actual_claim_amount <- sort(actual_claim_amount)

# Create a data frame for plotting
actual_data <- data.frame(
  CumulativePercentage = seq(0, 100, length.out = length(sorted_actual_claim_amount)),
  ClaimAmount = cumsum(sorted_actual_claim_amount)) 

# Calculate cumulative sum of expected claim amounts of GLM
cumulative_expected_claim_amount <- cumsum(claim_amount_expected_GLM)

# Create a data frame for plotting
expected_data <- data.frame(
  CumulativePercentage = seq(0, 100, length.out = length(cumulative_expected_claim_amount)),
  ClaimAmount = cumulative_expected_claim_amount)

# Calculate cumulative sum of expected claim amounts of GBM 
cumulative_expected_claim_amount_GBM <- cumsum(claim_amount_expected_GBM)

# Create a data frame for plotting
expected_data_GBM <- data.frame(
  CumulativePercentage = seq(0, 100, length.out = length(cumulative_expected_claim_amount_GBM)),
  ClaimAmount = cumulative_expected_claim_amount_GBM)


# Calculate cumulative sum of expected claim amounts of ANN
cumulative_expected_claim_amount_ANN <- cumsum(claim_amount_expected_ANN)

# Create a data frame for plotting
expected_data_ANN <- data.frame(
  CumulativePercentage = seq(0, 100, length.out = length(cumulative_expected_claim_amount_ANN)),
  ClaimAmount = cumulative_expected_claim_amount_ANN)

# Calculate cumulative sum of expected claim amounts of CANN
cumulative_expected_claim_amount_hybrid <- cumsum(claim_amount_expected_hybrid)

# Create a data frame for plotting
expected_data_hybrid <- data.frame(
  CumulativePercentage = seq(0, 100, length.out = length(cumulative_expected_claim_amount_hybrid)),
  ClaimAmount = cumulative_expected_claim_amount_hybrid)

# Create a line chart with actual and expected claim amounts
ggplot() +
  geom_line(data = actual_data, aes(x = CumulativePercentage, y = ClaimAmount, color = "Actual Data"),linetype = "solid", size = 1) +
  geom_line(data = expected_data, aes(x = CumulativePercentage, y = ClaimAmount, color = "GLM") ,linetype = "solid", size = 1)+
  geom_line(data = expected_data_GBM, aes(x = CumulativePercentage, y = ClaimAmount, color = "GBM"), linetype = "solid", size = 1)+
  geom_line(data = expected_data_ANN, aes(x = CumulativePercentage, y = ClaimAmount, color = "ANN"), linetype = "solid", size = 1)+
  geom_line(data = expected_data_hybrid, aes(x = CumulativePercentage, y = ClaimAmount, color = "Hybrid Model"), linetype = "solid", size = 1)+
  scale_color_manual(values = c("Actual Data" = "blue", "GLM" = "green", "GBM" = "red", "ANN" = "purple","Hybrid Model" = "yellow")) +  # Modify the color value here
  labs(x = "Percentage of Cases", y = "Loss Cost", title = "Actual Vs Expected Plots") +
  theme_minimal()


#--------------------------------s--------------END----------------------------------------------------------------------#


#---------------------------------------Models before Capping------------------------------------------s----#


#Remove duplicate rows

merged_data_uncapped <- merged_data

merged_data_uncapped<-unique(merged_data_uncapped)

nrow(merged_data_uncapped)

merged_data_uncapped <- subset(merged_data_uncapped, ClaimAmount != 0)


# Set the threshold as the 99.9th percentile
threshold_uncapped <- quantile(merged_data_uncapped$ClaimAmount, 0.9999)

# Replace values above the threshold with the threshold value
merged_data_uncapped$ClaimAmount <- pmin(merged_data_uncapped$ClaimAmount, threshold)

# Check the summary statistics after capping extreme values
summary(merged_data_uncapped$ClaimAmount)



#--------------------------------------END--------------------------------#

#Taking merged_data df to make modelss
set.seed(123)


ind <- initial_split(merged_data_uncapped, prop = 0.7)  # 70% for training, 30% for testing
train <- training(ind)
test <- testing(ind)
cat("The number of training set is ", nrow(train) ,"\n")
cat("The number of test set is", nrow(test))

#write.csv(test, file = "test.csv",row.names = FALSE )


#---------------------------------------------GLM model-----------------------------------------------------



neg_binom_glm_uncapped <- glm.nb(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus
                        + VehBrand + VehGas + Region + Area+ Density + offset(log(Exposure)), 
                        data = train)

summary(neg_binom_glm_uncapped)




#--------------------------------------------Claim Severity -----------------------------------------------------------#



gamma_model_uncapped <- glm(ClaimAmount ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region + Area
                            +offset(log(Exposure)),
                   data = train, family = Gamma(link = "log"))

summary(gamma_model_uncapped)


#-------------------------------------calculating loss cost------------------------------------------------------#

#Calculating expected claims from best freq model
pred_counts_GLM_uncapped <- predict(neg_binom_glm_uncapped, newdata = test, type = "response")

#calculating average claim cost from best severity model
pred_sev_GLM_uncapped <- predict(gamma_model_uncapped, newdata = test, type = "response")

#calculate expected claim amount by multiplying predictions from freq and sev model
claim_amount_expected_GLM_uncapped <- pred_sev_GLM_uncapped * pred_counts_GLM_uncapped

#calculate actual claim amount
actual_claim_amount_uncapped <- test$ClaimAmount * test$ClaimNb

# Calculate the loss for each observation
loss_cost_GLM_uncapped <- abs(claim_amount_expected_GLM_uncapped - actual_claim_amount_uncapped)


# Calculate the mean loss (loss cost) for all observations
mean_loss_cost_GLM_uncapped <- mean(loss_cost_GLM_uncapped)


