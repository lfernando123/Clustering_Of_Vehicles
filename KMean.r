# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(summarytools)
library(mice)
library(factoextra)
library(NbClust)


# Load the Excel file
data <- read.csv("vehicles.csv")
print(data)

# View basic structure and summary
str(data)
summary(data)

# Check for missing values
missing_data <- sapply(data, function(x) sum(is.na(x)))
print(missing_data)

# Impute missing values using mean for numeric columns
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(as.numeric(.), na.rm = TRUE), .)))


# Subset numeric columns
numeric_data <- data[, sapply(data, is.numeric)]

# Plot all boxplots together for outlier detection
boxplot(numeric_data, 
        main = "Boxplots of Numeric Columns", 
        las = 2,         # Rotate axis labels
        col = "lightgreen", 
        border = "darkgreen",
        cex.axis = 0.8)  # Adjust label size


# Function to cap outliers using IQR
treat_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

# Apply to numerical columns
numerical_cols <- data %>% select(where(is.numeric)) %>% names()
data <- data %>%
  mutate(across(all_of(numerical_cols), treat_outliers))


# Standardize numerical features
scaled_data <- data %>%
  select(all_of(numerical_cols)) %>%
  scale()  # Z-score normalization (mean=0, sd=1)


# Perform PCA
pca <- prcomp(scaled_data, center = TRUE, scale. = FALSE)  # Already scaled

# Scree plot to decide components
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50)) +
  labs(title = "Scree Plot")

# Choose optimal components (e.g., 95% variance)
cum_var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
optimal_components <- which(cum_var >= 0.95)[1]
pca_reduced <- pca$x[, 1:optimal_components]


# Extract PC values
pc_scores <- pca$x           # Projected data (samples x PCs)
loadings <- pca$rotation      # Variable loadings (features x PCs)
variance <- pca$sdev^2        # Variance explained by each PC

# View first 5 samples and 3 PCs
head(pc_scores[, 1:3], 5)

# Plot PC1 vs PC2
library(ggplot2)
ggplot(as.data.frame(pc_scores), aes(PC1, PC2)) +
  geom_point() +
  labs(title = "PCA Projection: PC1 vs PC2")


library(purrr)  # Provides map_dbl() function 

# Calculate WCSS for different k
wcss <- map_dbl(1:10, ~{
  kmeans(pca_reduced, centers = ., nstart = 10)$tot.withinss
})

# Plot Elbow Method
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "WCSS",
     main = "Elbow Method")


set.seed(123)
nc <- NbClust(pca_reduced,
min.nc=2, max.nc=15,
method="kmeans")
table(nc$Best.n[1,])


# Plot bar charts for the number of clusters chosen by 15 criteria
barplot(table(nc$Best.n[1,]), # provide bar charts####
xlab="Numer of Clusters",
ylab="Number of Criteria",
main="Number of Clusters Chosen by 15 Criteria")

# library for silhouette plot
library(cluster)

# Use first 2 principal components (PC1 and PC2)
pca_data <- as.data.frame(pca$x[, 1:2])

# Perform k-means clustering with 3 clusters (as an example)
set.seed(123)  # for reproducibility
km_res <- kmeans(pca_data, centers = 3, nstart = 25)

# Compute silhouette
sil <- silhouette(km_res$cluster, dist(pca_data))

# Plot silhouette
fviz_silhouette(sil)

# Perform k-means clustering with 2 clusters (as an example)
set.seed(123)  # for reproducibility
km_res <- kmeans(pca_data, centers = 2, nstart = 25)

# Compute silhouette
sil <- silhouette(km_res$cluster, dist(pca_data))

# Plot silhouette
fviz_silhouette(sil)

# Perform K-Means clustering with 2 clusters
set.seed(123)
kmeans_result <- kmeans(pca_data, centers = 2, nstart = 25)

# Add cluster assignments to data
pca_data$cluster <- as.factor(kmeans_result$cluster)

# Plot clusters in PCA space
fviz_cluster(kmeans_result, data = pca_data[, 1:2],
             ellipse.type = "norm",
             palette = "jco",
             ggtheme = theme_minimal())


# Convert to dataframe for plotting
loadings_df <- as.data.frame(loadings[, 1:2])
loadings_df$feature <- rownames(loadings_df)

# Melt for ggplot
library(reshape2)
loadings_melted <- melt(loadings_df, id.vars = "feature")

# Bar plot
ggplot(loadings_melted, aes(x = feature, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Feature Contributions to PC1 and PC2",
       y = "Loading Value", x = "Feature") +
  theme_minimal()