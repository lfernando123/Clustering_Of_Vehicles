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

# Use first 2 principal components (PC1 and PC2)
pca_data <- as.data.frame(pca$x[, 1:2])

# Compute distance matrix on PCA-reduced data
dist_matrix <- dist(pca_data)

# Apply hierarchical clustering (Ward's method is common for compact clusters)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc, main = "Dendrogram using Ward's Method", xlab = "", sub = "", cex = 0.6)

# Add rectangles to show cluster groups (choose 3 based on dendrogram cut)
rect.hclust(hc, k = 2, border = 2:4)

# Plot the each feature on the PCA space
fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Feature Contributions to Principal Components")