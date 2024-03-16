
#------------------------------------------VehAge-------------------------------------------------------#

#Plotting VehAgeAge distribution before capping
ggplot(merged_data, aes(x = VehAge)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Vehicle Age before Capping",
       x = "Vehicle Age",
       y = "Count")


#Plotting VehAgeAge distribution after capping
ggplot(filtered_data, aes(x = VehAge)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Vehicle Age after Capping",
       x = "Vehicle Age",
       y = "Count")



# VehAge Vs Freq before capping

# Vehicle Age vs Freq before capping
agg_data_freq <- merged_data %>%
  group_by(VehAge) %>%
  summarise(Count_VehAge = n(), Frequency = sum(ClaimNb) / sum(Exposure))

# Create a ggplot object
ggplot(agg_data_freq, aes(x = VehAge)) +
  geom_bar(aes(y = Count_VehAge), stat = "identity", fill = "blue") +
  geom_line(aes(y = Frequency / max(Frequency) * max(Count_VehAge)), color = "red") +
  geom_point(aes(y = Frequency / max(Frequency) * max(Count_VehAge)), color = "red") +
  scale_x_continuous(breaks = seq(10, max(agg_data_freq$VehAge), by = 10)) + # Set breaks for x-axis
  scale_y_continuous(sec.axis = sec_axis(~. * max(agg_data_freq$Frequency) / max(agg_data_freq$Count_VehAge),
                                         name = "Frequency")) +
  scale_color_manual(values = c("Vehicle Age" = "blue", "Number of Claims" = "green")) +  # Modify the color value here
  labs(title = "Observed Frequency per Vehicle Age Values before Capping",
       x = "Vehicle Age",
       y = "Count",
       ysec = "Frequency")


#Trends against Frequency after capping

# Aggregate data to get the count of vehicle age and frequency
agg_data_freq_after <- filtered_data %>%
  group_by(VehAge) %>%
  summarise(Count_VehAge = n(), Frequency = sum(ClaimNb) / sum(Exposure))

# Create a ggplot object
ggplot(agg_data_freq_after, aes(x = VehAge)) +
  geom_bar(aes(y = Count_VehAge), stat = "identity", fill = "blue") +
  geom_line(aes(y = Frequency / max(Frequency) * max(Count_VehAge)), color = "red") +
  geom_point(aes(y = Frequency / max(Frequency) * max(Count_VehAge)), color = "red") +
  scale_x_continuous(breaks = seq(10, max(agg_data_freq_after$VehAge), by = 10)) + # Set breaks for x-axis
  scale_y_continuous(sec.axis = sec_axis(~. * max(agg_data_freq_after$Frequency) / max(agg_data_freq_after$Count_VehAge),
                                         name = "Frequency")) +
  scale_color_manual(values = c("Vehicle Age" = "blue", "Frequency" = "green")) +  # Modify the color value here
  labs(title = "Observed Frequency per Vehicle Age Values after Capping",
       x = "Vehicle Age",
       y = "Count",
       ysec = "Frequency")




quantile(merged_data$VehAge, probs = seq(0, 1, by = 0.1))

quantile(merged_data$VehAge, probs = seq(0.9, 1, by = 0.01))

#-----------------------------------------------DrivAge------------------------------------------------#

#Plotting DrivAge distribution before capping

ggplot(merged_data, aes(x = DrivAge)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Driver Age before Capping",
       x = "Driver Age",
       y = "Count")

#Plotting DrivAge distribution after capping

ggplot(filtered_data, aes(x = DrivAge)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Driver Age after Capping",
       x = "Driver Age",
       y = "Count")


#------------------------------DrivAge Vs Frequency

# DrivAge Vs Freq before capping
agg_data_driveage_freq <- merged_data %>%
  group_by(DrivAge) %>%
  summarise(Count_DrivAge = n(), Frequency = sum(ClaimNb) / sum(Exposure))

# Create a ggplot object
ggplot(agg_data_driveage_freq, aes(x = DrivAge)) +
  geom_bar(aes(y = Count_DrivAge), stat = "identity", fill = "green") +
  geom_line(aes(y = Frequency / max(Frequency) * max(Count_DrivAge)), color = "red") +
  geom_point(aes(y = Frequency / max(Frequency) * max(Count_DrivAge)), color = "red") +
  labs(title = "Observed Frequency per Driver Age Values before Capping",
       x = "Driver Age",
       y = "Count") +
  scale_y_continuous(sec.axis = sec_axis(~. * max(agg_data_driveage_freq$Frequency) / max(agg_data_driveage_freq$Count_DrivAge),
                                         name = "Frequency")) +
  scale_x_continuous(breaks = seq(20, 80, by = 20)) + # Set breaks for x-axis
  theme_minimal()


# DrivAge Vs Freq after capping

# Aggregate data to get the count of driver age and total number of claims
agg_data_driveage_freq_after <- filtered_data %>%
  group_by(DrivAge) %>%
  summarise(Count_DrivAge = n(), Frequency = sum(ClaimNb) / sum(Exposure))

# Create a ggplot object
ggplot(agg_data_driveage_freq_after, aes(x = DrivAge)) +
  geom_bar(aes(y = Count_DrivAge), stat = "identity", fill = "green") +
  geom_line(aes(y = Frequency / max(Frequency) * max(Count_DrivAge)), color = "red") +
  geom_point(aes(y = Frequency / max(Frequency) * max(Count_DrivAge)), color = "red") +
  labs(title = "Observed Frequency per Driver Age Values after Capping",
       x = "Driver Age",
       y = "Count") +
  scale_y_continuous(sec.axis = sec_axis(~. * max(agg_data_driveage_freq$Frequency) / max(agg_data_driveage_freq_after$Count_DrivAge),
                                         name = "Frequency")) +
  scale_x_continuous(breaks = seq(20, 80, by = 20)) + # Set breaks for x-axis
  theme_minimal()



quantile(merged_data$DrivAge, probs = seq(0, 1, by = 0.1))

quantile(merged_data$DrivAge, probs = seq(0.9, 1, by = 0.01))

quantile(merged_data$DrivAge, probs = seq(0.9, 1, by = 0.001))


#----------------------------------------------VehPower------------------------------------------------#

#Plotting VehPower distribution before capping
ggplot(merged_data, aes(x = VehPower)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  labs(title = "Vehicle Power before Capping",
       x = "Vehicle Power",
       y = "Count")

#Plotting VehPower distribution after capping

ggplot(filtered_data, aes(x = VehPower)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  labs(title = "Vehicle Power after Capping",
       x = "Vehicle Power",
       y = "Count")

#Vehicle Power VS Freq before capping

# Aggregate data to get the count of vehicle power and total number of claims
agg_data_vepower_freq <- merged_data %>%
  group_by(VehPower) %>%
  summarise(Count_VehPower = n(), Frequency = sum(ClaimNb) / sum(Exposure))


ggplot(agg_data_vepower_freq, aes(x = VehPower)) +
  geom_bar(aes(y = Count_VehPower), stat = "identity", fill = "red") +
  geom_line(aes(y = Frequency / max(Frequency) * max(Count_VehPower)), color = "blue") +
  geom_point(aes(y = Frequency / max(Frequency) * max(Count_VehPower)), color = "blue") +
  labs(title = "Observed Frequency per Vehicle Power Values before Capping",
       x = "Vehicle Power",
       y = "Count") +
  scale_y_continuous(sec.axis = sec_axis(~. * max(agg_data_vepower_freq$Frequency) / max(agg_data_vepower_freq$Count_VehPower),
                                         name = "Frequency")) +
  theme_minimal()


#Vehicle Power VS Freq after capping

agg_data_vepower_freq_after <- filtered_data %>%
  group_by(VehPower) %>%
  summarise(Count_VehPower = n(), Frequency = sum(ClaimNb) / sum(Exposure))

# Create a ggplot object
ggplot(agg_data_vepower_freq_after, aes(x = VehPower)) +
  geom_bar(aes(y = Count_VehPower), stat = "identity", fill = "red") +
  geom_line(aes(y = Frequency / max(Frequency) * max(Count_VehPower)), color = "blue") +
  geom_point(aes(y = Frequency / max(Frequency) * max(Count_VehPower)), color = "blue") +
  labs(title = "Observed Frequency per Vehicle Power Values after Capping",
       x = "Vehicle Power",
       y = "Count") +
  scale_y_continuous(sec.axis = sec_axis(~. * max(agg_data_vepower_freq_after$Frequency) / max(agg_data_vepower_freq_after$Count_VehPower),
                                         name = "Frequency")) +
  theme_minimal()




quantile(merged_data$VehPower, probs = seq(0.9, 1, by = 0.001))


#-----------------------------------------BonusMalus--------------------------------------------------

#Bonusmalus distribution before and after capping
ggplot(merged_data, aes(x = BonusMalus)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  labs(title = "BonusMalus before Capping",
       x = "BonusMalus",
       y = "Count")

ggplot(filtered_data, aes(x = BonusMalus)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  labs(title = "BonusMalus after Capping",
       x = "BonusMalus",
       y = "Count")

#Banding BonusMalus

#Plotting BonusMalus distribution before capping

#creating bins
merged_data$bins_bm1 <- cut(merged_data$BonusMalus, breaks = c(0, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, Inf), include.lowest = TRUE)


#update bin labels
bin_labels1 <- c("50", "51-60", "61-70", "71-80", "81-90", "91-100", "101-110", "111-120", "121-130", "131-140", "141-150", "151-160", "161-170", "171-180", "181-190","191-200", "Above 200")

# Plotting with the corrected bin_labels
ggplot(merged_data, aes(x = bins_bm1)) +  
  geom_bar(stat = "count", fill = "dodgerblue") +  
  labs(
    title = "BonusMalus before Capping",
    x = "BonusMalus",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels = bin_labels)


#Plotting BonusMalus distribution after capping
filtered_data$bins_bm <- cut(filtered_data$BonusMalus, breaks = c(0, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150), include.lowest = TRUE)

#update bin labels
bin_labels <- c("50", "51-60", "61-70", "71-80", "81-90", "91-100", "101-110", "111-120", "121-130", "131-140", "141-150")

# Plotting with the corrected bin_labels
ggplot(filtered_data, aes(x = bins_bm)) +  
  geom_bar(stat = "count", fill = "dodgerblue") +  
  labs(
    title = "BonusMalus after Capping",
    x = "BonusMalus",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels = bin_labels)



#BonuMalus VS Freq before capping


# Aggregate data to get the count of BonusMalus and Frequency
agg_data_bonusmalus_freq <- merged_data %>%
  group_by(bins_bm1) %>%
  summarise(Count_bins_bm1 = n(), Frequency = sum(ClaimNb) / sum(Exposure))

ggplot(agg_data_bonusmalus_freq, aes(x = bins_bm1)) +
  geom_bar(aes(y = Count_bins_bm1), stat = "identity", fill = "red") +
  geom_line(aes(y = Frequency / max(Frequency) * max(Count_bins_bm1)), color = "blue", group = 1) +
  geom_point(aes(y = Frequency / max(Frequency) * max(Count_bins_bm1)), color = "blue") +
  labs(title = "Observed Frequency per BonusMalus Values before Capping",
       x = "BonusMalus",
       y = "Count") +
  scale_y_continuous(sec.axis = sec_axis(~. * max(agg_data_bonusmalus_freq$Frequency) / max(agg_data_bonusmalus_freq$Count_bins_bm1),
                                         name = "Frequency")) +
  scale_x_discrete(labels = bin_labels1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#BonuMalus VS Freq after capping
agg_data_bonusmalus_freq_after <- filtered_data %>%
  group_by(bins_bm) %>%
  summarise(Count_bins_bm = n(), Frequency = sum(ClaimNb) / sum(Exposure))

ggplot(agg_data_bonusmalus_freq_after, aes(x = bins_bm)) +
  geom_bar(aes(y = Count_bins_bm), stat = "identity", fill = "red") +
  geom_line(aes(y = Frequency / max(Frequency) * max(Count_bins_bm)), color = "blue", group = 1) +
  geom_point(aes(y = Frequency / max(Frequency) * max(Count_bins_bm)), color = "blue") +
  labs(title = "Observed Frequency per BonusMalus Values after Capping",
       x = "BonusMalus",
       y = "Count") +
  scale_y_continuous(sec.axis = sec_axis(~. * max(agg_data_bonusmalus_freq_after$Frequency) / max(agg_data_bonusmalus_freq_after$Count_bins_bm),
                                         name = "Frequency")) +
  scale_x_discrete(labels = bin_labels) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

quantile(merged_data$BonusMalus, probs = seq(0.9, 1, by = 0.0001))



#----------------------------------------------ClaimNb--------------------------------------------------#



#Plotting ClaimNb distribution before capping
barplot(table(merged_data$ClaimNb),
        main = "Claim Number before Capping",
        xlab = "Claim Number",
        ylab = "Count",
        col = "green"
)

#Plotting ClaimNb distribution after capping
barplot(table(filtered_data$ClaimNb),
        main = "Claim Number after Capping",
        xlab = "Claim Number",
        ylab = "Count",
        col = "green"
)

quantile(merged_data$ClaimNb, probs = seq(0.9, 1, by = 0.001))


#---------------------------------------------Exposure--------------------------------------------------#
 
 
#Plotting Exposure distribution before capping
hist(merged_data$Exposure, 
     main = "Exposure before Capping",  
     xlab = "Exposure", 
     ylab = "Count",
     col = "dodgerblue",  
     border = "black",  
     breaks = "FD"  # Method for determining the number of bins (histogram breaks)
)

#Plotting Exposure distribution before capping
hist(filtered_data$Exposure, 
     main = "Exposure after Capping",  
     xlab = "Exposure", 
     ylab = "Count",
     col = "dodgerblue",  
     border = "black",  
     breaks = "FD"  # Method for determining the number of bins (histogram breaks)
)
quantile(merged_data$ClaimNb,probs = seq(0.9,1,by= 0.001))



#------------------------Relationship Between Vehicle Age and Number of Claims-------------------------#

ggplot(filtered_data, aes(x = VehAge, y = ClaimNb)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Relationship Between Vehicle Age and Number of Claims",
    x = "Vehicle Age",
    y = "Number of Claims"
  ) +
  theme_minimal()


#----------------------Relationship Between Vehicle Power and Number of Claims--------------------------#

ggplot(filtered_data, aes(x = factor(VehPower), y = ClaimNb)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(
    title = "Relationship Between Vehicle Power and Number of Claims",
    x = "Vehicle Power",
    y = "Number of Claims"
  ) +
  theme_minimal() 

#-------------------------Relationship Between Driver Age and Number of Claims-------------------------#

# Bin the Vehicle Age
bins <- cut(filtered_data$DrivAge, breaks = c(0, 20,30,40,50,60,70,80,90), include.lowest = TRUE)

bin_labels <- c("Below 20", "20 to 30", "30 to 40", "40 to 50", "50 to 60", "60 to 70", "70 to 80", "Above 80")
# Create a bar plot
ggplot(filtered_data, aes(x = bins, y = ClaimNb)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Relationship Between Driver Age and Number of Claims",
    x = "Driver Age",
    y = "Number of Claims"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = bin_labels)


#--------------------------------------Correlation Analysis------------------------------------------#

install.packages("reshape2")
library(reshape2)


corr_mat <- round(cor(filtered_data[ c ("Area", 
                                      "VehPower", "VehAge", "DrivAge", "BonusMalus", "Density", "Exposure", "VehBrand", "VehGas", "Region")]),2)


# reorder corr matrix
# using corr coefficient as distance metric
dist <- as.dist((1-corr_mat)/2)

# hierarchical clustering the dist matrix
hc <- hclust(dist)
corr_mat <-corr_mat[hc$order, hc$order]

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)

print(corr_mat)

#plotting the correlation heatmap

ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() + theme(axis.text.x = element_text(angle = 90)) +
  geom_text(aes(label = value),
            color = "white", size = 4)  




