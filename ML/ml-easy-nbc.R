# Loading library
library(evclass)

# Step 1 - Collecting and Exploring the Data
data("ionosphere", package = "evclass")

# Take a look at the data
str(ionosphere)

# Class data
ionosphere$y

# Take a look at y

class_ionosphere <- factor(ionosphere$y, levels = c(1,2), labels = c("Good", "Bad"))
data <- as.data.frame(class_ionosphere)

library(ggplot2)
ggplot(data, aes(x = class_ionosphere)) +
  geom_bar(aes(fill = class_ionosphere, color = class_ionosphere))

data1 <- as.data.frame(ionosphere$x)
str(data1)
ggplot(data1, aes(x = V4)) +
  geom_density(binwidth = 0.1)
