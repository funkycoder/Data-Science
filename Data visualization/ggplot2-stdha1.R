#--------------------------
# INTRODUCTION TO ggplot2
#--------------------------

#Loading
library(ggplot2)

#Load data set
data("mtcars")
head(mtcars)

#Get only what you need
df <- mtcars[, c("mpg", "cyl", "wt")]

#Convert cyl to a factor variable
df$cyl <- as.factor(df$cyl)

#Print a sample of data
head(df)

# Scatter plot
#-------------
# Basic
qplot(x = mpg, y = wt, data = mtcars, geom = "point")

#With smooth line
qplot(x = mpg, y = wt, data = mtcars, geom = c("point","smooth"))

#with text
qplot(x = mpg, y = wt, data = mtcars, label = rownames(mtcars), geom = c("point","text"))

#Change the color by a continous numeric variable
qplot(mpg, wt, data = mtcars, color = cyl)

#Change color and shape by groups (factor)
mtcars$cyl <- factor(mtcars$cyl)
qplot(mpg, wt, data = mtcars, colour = cyl, shape = cyl)

#Change the size of points according to a continous variable
qplot(mpg, wt, data = mtcars, size = mpg)

# Box plot, histogram and density plots
#--------------------------------------
set.seed(1234)
wdata <- data.frame(sex = factor(rep(c("F", "M"), each = 200)), weight = c(rnorm(200,65), rnorm(200,58)))
head(wdata)

# Basic boxplot
# Change fill color by sex
qplot(sex, weight, data = wdata, geom = "boxplot")

#Basic histogram
qplot(weight, data = wdata, geom = "histogram")

#Density plot with main titles and axis labels
qplot(weight, data = wdata, geom = "density",
      xlab = "Weight (kg)", ylab = "Density", main = "Density plot")

# ggplot() function: Build plots piece by piece
# ===============================================
# Basic scatter plot
ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point()

# Change the point size and shape
ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = 1.5, shape = 18)
ggplot(data = mtcars, aes_string(x = "wt", y = "mpg")) + geom_point(size = 2, shape = 23)

# Use geometry function
ggplot(wdata, aes(x = weight)) + geom_density()
ggplot(wdata, aes(x = weight)) + stat_density()

# Adding layers to the plot
ggplot(data = mtcars, aes(x = wt, y = mpg)) + 
      geom_point() + # Points layer
      geom_line()    # Line layer

# Use different data and mapping for layers
ggplot(data = mtcars, aes(x = wt, y = mpg)) + 
  geom_point() + # Points layer
  geom_line(data = head(mtcars), color = "red")    # Line layer

# Do simple calculation in the aes function
ggplot(data = mtcars, aes(x = log2(wt), y = log2(mpg))) + geom_point()

# aes_tring() is used for aesthetic mappings from string objects
ggplot(data = mtcars, aes_string(x = "wt", y = "mpg")) + geom_point(color = "red") + geom_smooth()

# Helper function for creating a scatter plot
#--------------------------------------------
# data = data.frame
# xName, yName : specify x and y variable respectively
ggpoints <- function(data, xName, yName) {
 p <- ggplot(data = data, aes_string(xName, yName)) + geom_point(color = "red") + geom_smooth()
 return (p)
}

# Create a scatter plot using the helper function ggpoints()
ggpoints(mtcars, xName = "wt", yName = "mpg")

# Print the plot to a pdf file
pdf("presentation/myplot.pdf")
myplot <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
print(myplot)
dev.off()

# Print to a png file
png("presentation/myplot.png")
print(myplot)
dev.off()

# It's possible to make a ggplot on the screen then save it using ggsave
# Create a plot
ggplot(mtcars, aes(wt, mpg)) + geom_point()
ggsave("myplot.pdf")
ggsave("myplot.png")