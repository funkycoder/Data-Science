# Galton's Data
#-------------------------------------------------------------------------------------------------------------
# Let's look at the data first, used by Francis Galton in 1885. 
# Galton was a statistician who invented the term and concepts
# of regression and correlation, founded the journal Biometrika,
# and was the cousin of Charles Darwin.
# You may need to run `install.packages("UsingR")` if the `UsingR` library is not installed.
# Let's look at the marginal (parents disregarding children and children disregarding parents) distributions first. 
# Parent distribution is all heterosexual couples.
# Correction for gender via multiplying female heights by 1.08.
# Overplotting is an issue from discretization.

library(UsingR); data(galton); library(reshape); long <- melt(galton)
g <- ggplot(long, aes(x = value, fill = variable)) 
g <- g + geom_histogram(colour = "black", binwidth=1) 
g <- g + facet_grid(. ~ variable)
g

## Experiment
### Use R studio's manipulate to see what value of $\mu$ minimizes the sum of the squared deviations.
library(manipulate)
myHist <- function(mu){
  mse <- mean((galton$child - mu)^2)
  g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth=1)
  g <- g + geom_vline(xintercept = mu, size = 3)
  g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
  g
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

# The least squares est. is the empirical mean
g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth = 1)
g <- g + geom_vline(xintercept = mean(galton$child), size = 3)
g

# Comparing childrens' heights and their parents' heights
ggplot(galton, aes(x = parent, y = child)) + geom_point()

# Size of point represents number of points at that (X, Y) combination (See the Rmd file for the code).

library(dplyr)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
# Ignoring unknown aesthetics: show_guide
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g

# Regression through the origin 
# --------------------------------------------------------------------------------
# Suppose that $X_i$ are the parents' heights. 
# Consider picking the slope $\beta$ that minimizes $$\sum_{i=1}^n (Y_i - X_i \beta)^2$$ 
# This is exactly using the origin as a pivot point picking the line
# that minimizes the sum of the squared vertical distances of the points to the
# line Use R studio's  manipulate function to experiment Subtract the means so
# that the origin is the mean of the parent and children's heights

y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                     
g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
mse <- mean( (y - beta * x) ^2 )
g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

# The solution 
# In the next few lectures we'll talk about why this is the solution
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
lm1 <- lm(galton$child ~ galton$parent)
g <- g + geom_abline(intercept = coef(lm1)[1], slope = coef(lm1)[2], size = 3, colour = grey(.5))
g