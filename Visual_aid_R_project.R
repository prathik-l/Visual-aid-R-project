student_data <- data.frame(
  student_id = 1:17,
  without_visual_aids = c(50, 60, 58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61),
  with_visual_aids = c(58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65, 70)
)

# Inspect the structure of the data frame
str(student_data)

# Create a boxplot comparing scores with and without visual aids
boxplot(
  student_data$without_visual_aids, student_data$with_visual_aids,
  names = c("Without Visual Aids", "With Visual Aids"),
  main = "Comparison of Scores",
  ylab = "Scores",
  col = c("orange", "yellow")
)

# Calculate score differences between the two conditions
score_diff <- student_data$with_visual_aids - student_data$without_visual_aids
shapiro.test(student_data$without_visual_aids)
shapiro.test(student_data$with_visual_aids)

# Combine scores from both groups into one vector
all_scores <- c(student_data$ without_visual_aids, student_data$with_visual_aids)

# Conduct a normality test on the combined scores
combined_normality <- shapiro.test(all_scores)
combined_normality
# Calculate mean and median of combined scores
mean_all_scores <- mean(all_scores)
median_all_scores <- median(all_scores)
mean_all_scores  # The mean of all scores
median_all_scores  # The median of all scores

# Install and load the e1071 package if not already installed
if (!require(e1071)) {
  install.packages("e1071")
  library(e1071)
}

# Calculate skewness of the combined scores
skewness_all_scores <- skewness(all_scores)
skewness_all_scores  # Skewness of all scores

# Calculate the mean and standard deviation for both conditions
mean_no_aids <- mean(student_data$without_visual_aids)
mean_visual_aids <- mean(student_data$with_visual_aids)
sd_no_aids <- sd(student_data$without_visual_aids)
sd_visual_aids <- sd(student_data$with_visual_aids)

# Perform a paired t-test comparing the two conditions
paired_t_test <- t.test(student_data$without_visual_aids, student_data$with_visual_aids, paired = TRUE)
paired_t_test  # Output of the t-test

