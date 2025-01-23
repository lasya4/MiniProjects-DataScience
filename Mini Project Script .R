# Load necessary libraries
library(ggplot2)
# Load the dataset
wine_data <- read.csv("C:\\Users\\lasya\\OneDrive\\Desktop\\winequality-red.csv", sep = ";") # Change path as needed
# 1. Calculate the average alcohol content
avg_alcohol <- mean(wine_data$alcohol, na.rm = TRUE)
print(avg_alcohol)
write.csv(data.frame(avg_alcohol = avg_alcohol), "C:\\Users\\lasya\\OneDrive\\Desktop\\Q1.csv")

# 2. Find the maximum and minimum pH values
max_ph <- max(wine_data$pH, na.rm = TRUE)
min_ph <- min(wine_data$pH, na.rm = TRUE)
cat("Maximum pH:", max_ph, "\n")
cat("Minimum pH:", min_ph, "\n")
write.csv(data.frame(max_ph = max_ph, min_ph = min_ph), "C:\\Users\\lasya\\OneDrive\\Desktop\\Q2.csv")

# 3. Calculate the standard deviation of residual sugar
sd_residual_sugar <- sd(wine_data$residual.sugar, na.rm = TRUE)
print(sd_residual_sugar)
write.csv(data.frame(sd_residual_sugar = sd_residual_sugar), "C:\\Users\\lasya\\OneDrive\\Desktop\\Q3_sd_residual_sugar.csv")


# 4. Find the maximum and minimum values of each column
max_values <- sapply(wine_data, max, na.rm = TRUE)
min_values <- sapply(wine_data, min, na.rm = TRUE)
# Print the maximum and minimum values
cat("Maximum values:\n")
print(max_values)
cat("\nMinimum values:\n")
print(min_values)
# Write the results to a CSV file
write.csv(data.frame(max_values = max_values, min_values = min_values), "C:\\Users\\lasya\\OneDrive\\Desktop\\max_min_values.csv")

#5. Select only numeric columns for correlation calculation
numeric_cols <- sapply(wine_data, is.numeric)
cor_matrix <- cor(as.matrix(wine_data[, numeric_cols]), use = "complete.obs")
# Write the correlation matrix to a CSV file
write.csv(cor_matrix, "C:\\Users\\lasya\\OneDrive\\Desktop\\correlation_matrix_O5.csv")

#6.
# Plot the heatmap
library(ggplot2)
library(reshape2)
melted_cor_matrix <- melt(cor_matrix)
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "lightblue", high = "brown", mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("C:\\Users\\lasya\\OneDrive\\Desktop\\correlation_heatmap_O6.pdf", plot = p1, device = "pdf", width = 10, height = 8)

# 7. Find the number of wines with quality greater than 6
high_quality_wines <- sum(wine_data$quality > 6, na.rm = TRUE)
print(high_quality_wines)
write.csv(data.frame(high_quality_wines = high_quality_wines), "C:\\Users\\lasya\\OneDrive\\Desktop\\high_quality_wines.csv")

#8.# Plot the histogram for 'fixed acidity'
p1 <- ggplot(wine_data, aes(x = `fixed.acidity`)) +
  geom_histogram(binwidth = 0.5, fill = "brown", color = "lightblue") +
  labs(title = "Histogram of Fixed Acidity", x = "Fixed Acidity", y = "Frequency")
ggsave("C:\\Users\\lasya\\OneDrive\\Desktop\\Fixed Acidity.pdf", plot = p1, device = "pdf", width = 10, height = 8)
print(p1)

#9.# Plot the relationship between 'alcohol' and 'quality'
p2 <- ggplot(wine_data, aes(x = alcohol, y = quality)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Alcohol Content and Quality", x = "Alcohol", y = "Quality")
ggsave("C:\\Users\\lasya\\OneDrive\\Desktop\\Alcohol and quality.pdf", plot = p2, device = "pdf", width = 10, height = 8)
print(p2)

#10. Plot a boxplot of 'alcohol' content by 'quality'
p3 <- ggplot(wine_data, aes(x = as.factor(quality), y = alcohol)) +
  geom_boxplot() +
  labs(title = "Boxplot of Alcohol Content by Quality", x = "Quality", y = "Alcohol")
ggsave("C:\\Users\\lasya\\OneDrive\\Desktop\\Boxplot of Alcohol Content.pdf", plot = p3, device = "pdf", width = 10, height = 8)
print(p3)


# 11. Bar plot of average quality by alcohol content levels
p4 <- ggplot(wine_data, aes(x = cut(alcohol, breaks = 5), y = quality)) +
  stat_summary(fun = mean, geom = "bar", fill = "steelblue") +
  labs(title = "Average Quality by Alcohol Content Levels", x = "Alcohol Content Level", y = "Average Quality")
ggsave("C:\\Users\\lasya\\OneDrive\\Desktop\\content level.pdf", plot = p4, device = "pdf", width = 10, height = 8)
print(p4)

# 12. Density plot for sulphates across different qualities
p5 <- ggplot(wine_data, aes(x = sulphates, fill = as.factor(quality))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density of Sulphates Across Different Qualities", x = "Sulphates", y = "Density") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()
ggsave("C:\\Users\\lasya\\OneDrive\\Desktop\\sulphates.pdf", plot = p5, device = "pdf", width = 10, height = 8)
print(p5)

#13.
# Perform ANOVA on quality by alcohol content
anova_result <- aov(quality ~ alcohol, data = wine_data)
summary(anova_result)
write.csv(summary(anova_result), file = "C:\\Users\\lasya\\OneDrive\\Desktop\\anova_result.csv")

#14
# Perform a t-test on alcohol content between high and low-quality wines
high_quality_alcohol <- wine_data$alcohol[wine_data$quality > 6]
low_quality_alcohol <- wine_data$alcohol[wine_data$quality <= 6]
t_test_result <- t.test(high_quality_alcohol, low_quality_alcohol)
t_test_result

#15

# Select columns for K-means clustering
selected_cols <- wine_data[, c("fixed.acidity", "volatile.acidity", "citric.acid", 
                               "residual.sugar", "chlorides", "free.sulfur.dioxide", 
                               "total.sulfur.dioxide", "density", "pH", "sulphates", 
                               "alcohol")]

# Perform K-means clustering
kmeans_result <- kmeans(selected_cols, centers = 3, nstart = 25)

# Get cluster assignments
cluster_assignments <- kmeans_result$cluster

# Add cluster assignments to the dataset
wine_data$cluster <- cluster_assignments

# Write the clustered dataset to a CSV file
write.csv(wine_data, "clustered_data.csv")
library(ggplot2)

# Scatter plot of alcohol vs pH, colored by cluster
ggplot(wine_data, aes(x = alcohol, y = pH, color = factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering of Wine Data", x = "Alcohol", y = "pH", color = "Cluster") +
  theme_minimal()
ggsave("C:\\Users\\lasya\\OneDrive\\Desktop\\Clustereddata.pdf", plot = p5, device = "pdf", width = 10, height = 8)



