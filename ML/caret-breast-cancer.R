# Loading library

# Preparing data
bc_data <- read.table("ML/data/breast-cancer-wisconsin.data",
                      header = FALSE, sep = ",")
colnames(bc_data) <- c("sample_code_number",
                       "clump_thickness",
                       "uniformity_of_cell_size",
                       "uniformity_of_cell_shape",
                       "marginal_adhesion",
                       "single_epithelial_cell_size",
                       "bare_nuclei",
                       "bland_chromatin",
                       "normal_nucleoli",
                       "mitosis",
                       "classes")
bc_data$classes <- ifelse(bc_data$classes == "2", "benign",
                          ifelse(bc_data$classes == "4", "malignant", NA))
summary(bc_data)

# Missing data?
bc_data[bc_data == "?"] <- NA
sum(is.na(bc_data))

# Change to numeric from field 2 to 10
bc_data[, 2:10] <- apply(bc_data[, 2:10], 2, function(x) as.numeric(as.character(x)))
str(bc_data)

library(mice)
dataset_impute <- mice(bc_data[, 2:10], print = FALSE) #?
train_data <- cbind(bc_data[, 11, drop = FALSE], complete(dataset_impute, 1))
summary(train_data)

library(ggplot2)
ggplot(bc_data, aes(x = classes, fill = classes)) +
      geom_bar() +
      ggtitle("Outcome variable")

# Checking for correlation
library(corrplot)
cor_data <- train_data[, -1]
cor_matrix <- cor(data.matrix(cor_data))
corrplot(cor_matrix)
library(caret)
hi_cor <- findCorrelation(cor_matrix, cutoff = 0.7, exact = FALSE)
hi_cor
colnames(cor_data[hi_cor])

# Remove one of the highly correlated variable
train_data$uniformity_of_cell_size <- NULL

# Create training and testing data set
set.seed(42)
index <- createDataPartition(train_data$classes, p = 0.75, list = FALSE)
train_set <- train_data[index,]
test_set <- train_data[-index,]

library(tidyr)
rbind(data.frame(group = "train", train_set), data.frame(group = "test", test_set)) %>% 
  gather(x, y, marginal_adhesion : mitosis) %>% 
  ggplot(aes(x = y, color = group, fill = group)) +
    geom_density(alpha = 0.3) +
    ggtitle("Variable density by groups") +
    facet_wrap(~ x, scales = "free",  ncol =  3)