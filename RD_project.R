# The following script is for a research project examining the relationship between racial discrimination and anxiety/PTSD symptoms in Black youth
# The script includes univariate statistics, descriptive statistics, and inferential statistics

# set the working directory
setwd("/Users/amanpreetbhogal/Documents/2024-2025/Research_2024_2025/RD_project/RD_data")

# import data
abcd_data <- read.csv("abcd_data.csv")

# install ggplot2 and load it
install.packages("ggplot2")
library(ggplot2)

# mean and standard deviation for age
mean(abcd_data$age)
sd(abcd_data$age)

# frequency analyses for percentage male vs female 
frequency(abcd_data$sex) # EVALUATE THIS CODE

# Create Histograms
## Histogram PRACY - Racial Discrimination
ggplot(data = abcd_data, mapping = aes(x = pracy_sum_c_teensurvey_combined)) +
  geom_histogram(color = "skyblue", fill = "pink") +
  labs(
    x = "PRACY",
    y = "count",  
    title = "PRACY Histogram" 
  )
## Histogram SCARED - Anxiety Symptoms
ggplot(data = abcd_data, mapping = aes(x = abcd_data$scared_sum_c_teensurvey)) +
  geom_histogram(color = "skyblue", fill = "pink") +
  labs(
    x = "SCARED",  
    y = "count", 
    title = "SCARED Histogram"  
  )
## Histogram PTSD Symptoms
ggplot(data = abcd_data, mapping = aes(x = abcd_data$ptss_total)) +
  geom_histogram(color = "skyblue", fill = "pink") +
  labs(
    x = "PTSS Total",
    y = "count",  
    title = "PTSS Histogram"  
  )

# Calculate Z-scores for each variable 
scared_zscore <- scale(abcd_data$scared_sum_c_teensurvey)
pracy_zscore <- scale(abcd_data$pracy_sum_c_teensurvey_combined)
ptss_total_zscore <- scale(abcd_data$ptss_total_teen)

# Correlation Matrix
## Install Hmisc package to get the correlation coefficient and the p-values
install.packages("Hmisc")
library(Hmisc)

## Use the rcorr function 
cor_results <- rcorr(as.matrix(abcd_data[, c("pracy_sum_c_teensurvey_combined", "scared_sum_c_teensurvey",
                                                   "ptss_total_teen")]))
## Extract the correlation matrix
cor_matrix <- cor_results$r

## Extract p-values
p_values <- cor_results$P

## Combine correlation matrix and p-values into a single data frame
corr_combined_results <- data.frame(
  Variable1 = rep(rownames(cor_matrix), times=ncol(cor_matrix)),
  Variable2 = rep(colnames(cor_matrix), each=nrow(cor_matrix)),
  Correlation = as.vector(cor_matrix),
  P_Value = as.vector(p_values)
)

## Export the combined results to a CSV file
write.csv(corr_combined_results, "combined_correlation_p_values.csv", row.names = FALSE)

# Linear Regression
# Plot the results using sjPlot
install.packages("sjPlot")
library(sjPlot)
# Linear Regression PRACY and SCARED
lm_scared_pracy <- lm(scared_sum_c_teensurvey ~ pracy_sum_c_teensurvey_combined + age_teensurvey + sex, data = abcd_data)
tab_model(lm_scared_pracy)
# Linear Regression PRACY and PTSS
lm_ptss_pracy <- lm(ptss_total_teen ~ pracy_sum_c_teensurvey_combined + age_teensurvey + sex, data = abcd_data)
tab_model(lm_ptss_pracy)


