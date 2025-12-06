#CHALLENGE 1 
# Set your working directory 
setwd("~/Desktop/R/Summative")  

# Load necessary libraries
library(dplyr)

# Load the CSV files
age_group <- read.csv("cw_data_age_group.csv")
individual_differences <- read.csv("cw_data_ind_diff.csv")
percept <- read.csv("cw_data_percept.csv")

# Check column names for each dataset
colnames(age_group)
colnames(individual_differences)
colnames(percept)

# Ensure 'Participant' exists in all datasets
# If 'age_group' does not have a Participant column, create a placeholder
if (!"Participant" %in% colnames(age_group)) {
  age_group$Participant <- seq_len(nrow(age_group))  # Temporary numeric ID
}

# Ensure the Participant column exists in all datasets before merging
individual_differences$Participant <- percept$Participant
if (!"Participant" %in% colnames(individual_differences)) {
  stop("Error: 'Participant' column is missing in 'individual_differences'. Check data structure.")
}
percept$Participant <- percept$Participant
if (!"Participant" %in% colnames(percept)) {
  stop("Error: 'Participant' column is missing in 'percept'. Check data structure.")
}

# Merge datasets using 'Participant' as the key
df <- percept %>% 
  full_join(individual_differences, by = "Participant") %>%
  full_join(age_group, by = "Participant")

# Save the combined dataset
write.csv(df, "combined_data.csv", row.names = FALSE)

# Print structure to verify
str(df)


#CHALLENGE 2 
library(ggplot2)
summary(df$PTA_Left)
sd(df$PTA_Left)
summary(df$PTA_Right)
sd(df$PTA_Right)

# Correlation analysis for males
male_df <- df %>% filter(Sex == "M")
male_cor <- cor.test(male_df$PTA_Left, male_df$PTA_Right, use = "complete.obs")
print(male_cor)  # Print the result to the console
summary(male_df$PTA_Left)
sd(male_df$PTA_Left)
summary(male_df$PTA_Right)
sd(male_df$PTA_Right)

# Correlation analysis for females
female_df <- df %>% filter(Sex == "F")
female_cor <- cor.test(female_df$PTA_Left, female_df$PTA_Right, use = "complete.obs")
print(female_cor)  # Print the result to the console
summary(female_df$PTA_Left)
summary(female_df$PTA_Right)
sd(female_df$PTA_Left)
sd(female_df$PTA_Right)

# Create scatterplot
ggplot(df, aes(x = PTA_Left, y = PTA_Right, color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between PTA_Left and PTA_Right by Sex", 
       x = "PTA Left Ear Score", 
       y = "PTA Right Ear Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save the scatterplot
ggsave("PTA_scatterplot.png", width = 8, height = 6)

# Output results for APA formatting
cat("Results\n")
cat("For male participants, there was a", ifelse(male_cor$p.value < 0.05, "significant", "non-significant"),
    "correlation between PTA_Left and PTA_Right, r =", round(male_cor$estimate, 2),
    ", p =", round(male_cor$p.value, 3), ".\n")

cat("For female participants, there was a", ifelse(female_cor$p.value < 0.05, "significant", "non-significant"),
    "correlation between PTA_Left and PTA_Right, r =", round(female_cor$estimate, 2),
    ", p =", round(female_cor$p.value, 3), ".\n")

##CHALLENGE 3
library(ggplot2)
install.packages("ez")
library(ez)
install.packages("emmeans")
library(emmeans)
library(tidyr)
library(tidyverse)


head(age_group)
head(individual_differences)
head(percept)

#Reshape the data to long format using gather()
percept_long <- percept %>%
  gather(key = "condition", value = "accuracy", Acc_Clear, Acc_Noise, Acc_Vocoded, Acc_Compressed)

# Conduct a one-way repeated measures ANOVA using aov()
anova_results <- aov(accuracy ~ condition + Error(Participant/condition), data = percept_long)
print(anova_results)
# Obtain relevant effect size for the ANOVA main effect result
effect_size <- anova_results$ANOVA$ges[anova_results$ANOVA$Effect == "condition"]

# Function to conduct pairwise t-tests
pairwise_t_test_results <- function(cond1, cond2) {
  df1 <- na.omit(percept_long$accuracy[percept_long$condition == cond1])
  df2 <- na.omit(percept_long$accuracy[percept_long$condition == cond2])
  
  # Ensure both datasets have values
  if (length(df1) == 0 || length(df2) == 0 ) {
    return(c(NA, NA, NA, NA, NA))
  }
  
  # Conduct the t-test
  test <- t.test(df1, df2, paired = TRUE)
  
  # Return results including the confidence intervals
  return(c(test$statistic, test$p.value, test$estimate, test$conf.int))
}

# Initialize results data frame with all expected columns
results <- data.frame(
  Comparison = character(),
  t_statistic = numeric(),
  p_value = numeric(),
  Mean_Difference = numeric(),
  SD_i = numeric(), #The standard deviation for the first condition.
  SD_j = numeric(), #The standard deviation for the second condition. 
  Cohen_d = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

# List of conditions
conditions <- unique(percept_long$condition)

# Loop through combinations of conditions
for (i in 1:(length(conditions) - 1)) {
  for (j in (i + 1):length(conditions)) {
    test_result <- pairwise_t_test_results(conditions[i], conditions[j])
    
    # Calculate means and standard deviations for both conditions
    mean_i <- mean(percept_long$accuracy[percept_long$condition == conditions[i]], na.rm = TRUE)
    mean_j <- mean(percept_long$accuracy[percept_long$condition == conditions[j]], na.rm = TRUE)
    
    sd_i <- sd(percept_long$accuracy[percept_long$condition == conditions[i]], na.rm = TRUE)
    sd_j <- sd(percept_long$accuracy[percept_long$condition == conditions[j]], na.rm = TRUE)
    
    # Calculate pooled standard deviation
    n_i <- length(percept_long$accuracy[percept_long$condition == conditions[i]])
    n_j <- length(percept_long$accuracy[percept_long$condition == conditions[j]])
    
    pooled_sd <- sqrt(((n_i - 1) * sd_i^2 + (n_j - 1) * sd_j^2) / (n_i + n_j - 2))
    
    # Calculate Cohen's d
    cohen_d <- (mean_i - mean_j) / pooled_sd
    
    # Check if the result is valid
    if (!is.na(test_result[1])) {
      results <- rbind(results, data.frame(
        Comparison = paste(conditions[i], "vs", conditions[j]),
        t_statistic = test_result[1],
        p_value = test_result[2],
        Mean_Difference = mean_i - mean_j,
        SD_i = sd_i,
        SD_j = sd_j,
        Cohen_d = cohen_d,
        CI_Lower = test_result[4],
        CI_Upper = test_result[5]
      ))
    }
  }
}

# Print results
print(results)


# Create the boxplot using ggplot2
ggplot(percept_long, aes(x = condition, y = accuracy)) +
  geom_boxplot(notch = TRUE, outlier.colour = "black", colour = "black") +
  geom_jitter(aes(colour = condition), width = 0.2) +
  scale_colour_manual(values = c("Acc_Clear" = "red", "Acc_Compressed" = "blue", "Acc_Noise" = "green", "Acc_Vocoded" = "purple")) +
  labs(x = "Condition", y = "Accuracy (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 100, by = 10))

# Save and export the figure
ggsave("accuracy_boxplot.png")


# Include the figure in your Results document
cat("Figure 1 shows the boxplot of accuracy across the four conditions.\n")

## CHALLENGE 4 
library(tidyr)
library(ggplot2)

# Reshape the data to long format
response_long <- df %>%
  pivot_longer(cols = starts_with("RT_"), 
               names_to = "condition", 
               values_to = "response_time")

# Conduct the two-way mixed ANOVA
mixed_anova <- aov(response_time ~ condition * age_group + Error(Participant/condition), data = response_long)

# View the summary of the ANOVA
summary(mixed_anova)

# Calculate effect size
library(effectsize)
eta_squared(mixed_anova)

# Initialize results data frame with all expected columns
results <- data.frame(
  Comparison = character(),
  t_statistic = numeric(),
  p_value = numeric(),
  Mean_Difference = numeric(),
  SD_i = numeric(), # The standard deviation for the first condition
  SD_j = numeric(), # The standard deviation for the second condition
  Cohen_d = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

# List of conditions
conditions <- unique(response_long$condition)

# Loop through combinations of conditions
for (i in 1:(length(conditions) - 1)) {
  for (j in (i + 1):length(conditions)) {
    # Perform the t-test
    test_result <- t.test(response_time ~ condition, 
                          data = response_long[response_long$condition %in% c(conditions[i], conditions[j]), ])
    
    # Calculate means and standard deviations for both conditions
    mean_i <- mean(response_long$response_time[response_long$condition == conditions[i]], na.rm = TRUE)
    mean_j <- mean(response_long$response_time[response_long$condition == conditions[j]], na.rm = TRUE)
    
    sd_i <- sd(response_long$response_time[response_long$condition == conditions[i]], na.rm = TRUE)
    sd_j <- sd(response_long$response_time[response_long$condition == conditions[j]], na.rm = TRUE)
    
    # Calculate pooled standard deviation
    n_i <- length(response_long$response_time[response_long$condition == conditions[i]])
    n_j <- length(response_long$response_time[response_long$condition == conditions[j]])
    
    pooled_sd <- sqrt(((n_i - 1) * sd_i^2 + (n_j - 1) * sd_j^2) / (n_i + n_j - 2))
    
    # Calculate Cohen's d
    cohen_d <- (mean_i - mean_j) / pooled_sd
    
    # Check if the result is valid
    if (!is.na(test_result$p.value)) {
      results <- rbind(results, data.frame(
        Comparison = paste(conditions[i], "vs", conditions[j]),
        t_statistic = test_result$statistic,
        p_value = test_result$p.value,
        Mean_Difference = mean_i - mean_j,
        SD_i = sd_i,
        SD_j = sd_j,
        Cohen_d = cohen_d,
        CI_Lower = test_result$conf.int[1],
        CI_Upper = test_result$conf.int[2]
      ))
    }
  }
}

# Print results
print(results)

# Calculate mean and standard deviation for each condition and age group
summary_data <- response_long %>%
  group_by(condition, age_group) %>%
  summarise(
    mean_response = mean(response_time, na.rm = TRUE),
    sd_response = sd(response_time, na.rm = TRUE),
    .groups = 'drop'
  )

# Creating factor so that graph goes in chronological age order from youngest to olest 
summary_data$age_group <- factor(summary_data$age_group, levels = c("Younger", "Middle", "Older"))

#Create bar chart
ggplot(summary_data, aes(x = condition, y = mean_response, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - sd_response, ymax = mean_response + sd_response),
                width = 0.5, linewidth = 1, position = position_dodge(0.7)) +
  facet_wrap(~ age_group) +
  scale_fill_manual(values = c("RT_Clear" = "cyan", 
                               "RT_Compressed" = "orange", 
                               "RT_Noise" = "lightgreen", 
                               "RT_Vocoded" = "lightblue")) +
  labs(x = "Conditions", y = "Response Time (ms)") +
  scale_x_discrete(labels = c("RT_Clear" = "Clear", 
                              "RT_Compressed" = "Comp", 
                              "RT_Noise" = "SPiN", 
                              "RT_Vocoded" = "Voco")) +
  theme_minimal() +  
  scale_y_continuous(breaks = seq(0, 1400, by = 200))

# Save the figure
ggsave("response_times_bar_chart.png", width = 10, height = 6)

