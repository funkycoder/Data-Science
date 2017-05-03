# Generate some data (wdata) containing weight by sex (M/F)
set.seed(1234)
wdata <- data.frame( sex = factor(rep(c("F", "M"), each = 200)),
                     weight = c(rnorm(200, 55), rnorm(200, 58))
                     )
head(wdata)

# Calculate the weight mean value by sex
library(dplyr)
mu <- wdata %>% 
      group_by(sex) %>% 
      summarise(grp.mean = mean(weight))
head(mu)

# Creat a plot
library(ggplot2)
g <- ggplot(wdata, aes(x = weight))
g + geom_area(stat = "bin", color = "black", fill = "#00AFBB")

g + geom_area(aes(y = ..density..), stat = "bin")

#Load diamonds data from ggplot2
data(diamonds)
p <- ggplot(diamonds, aes(x = price, fill = cut))
# Bar plot
p + geom_bar(stat = "bin")
# Area_plot
p + geom_area(stat = "bin")

# Basic density plot
g + geom_density()
# Change line and fill color, add mean ref line
g + geom_density(color = "black", fill = "gray") +
    geom_vline(aes(xintercept = mean(weight), color = "#FC4E07", 
                   linetype = "dashed", size = 1))

# Change line colors by sex
g + geom_density(aes(color = sex))
# Change fill color by sex and use semi-transparent
g + geom_density(aes(fill = sex), alpha = 0.4)
# Add mean line and color by sex
g + geom_density(aes(color = sex), alpha = 0.4) +
    geom_vline(data = mu, aes(xintercept = grp.mean, color = sex), linetype = "dashed")

# Change color by group
g2 <- g + geom_density(aes(color = sex)) + 
          geom_vline(data = mu, aes(xintercept = grp.mean, color = sex), linetype = "dashed") + 
          theme_minimal()
g2 + scale_color_manual(values = c("#999999", "#E69F00"))
# Use brewer pallete
g2 + scale_color_brewer(palette = "Paired")
# Use gray scale
g2 + scale_color_grey()

# Change manually fill color
g3 <- g + geom_density(aes(fill = sex), alpha = 0.4) + theme_minimal()
g3 + scale_fill_manual(values = c("#999999", "#E69F00"))
g3 + scale_fill_brewer(palette = "Dark2") + theme_minimal()
g3 + scale_fill_grey()

#############################################################
#       HISTOGRAM
#############################################################
# Basic plot
g + geom_histogram()
# Change number of bins
g + geom_histogram(bins = 50)
# Change line color, fill color, mean line
g + geom_histogram(color = "black", fill = "gray") +
    geom_vline(aes(xintercept = mean(weight), color = "#FC4E07"), linetype = "dashed", size = 1)
# Density
g + geom_histogram(aes(y = ..density..))
# Change line color by sex
g + geom_histogram(aes(color = sex), fill = "white", alpha = 0.6, position = "identity")
# Position adjustment: "dodge" (Interleaved)
g + geom_histogram(aes(color = sex), fill = "white", position = "dodge") +
    geom_vline(data = mu, aes(xintercept = grp.mean, color = sex), linetype = "dashed")
# Manually change color
g + geom_histogram(aes(color = sex), fill = sex, alpha = 0.4, position = "identity") +
    scale_color_manual(values = c("#00AFBB", "#E7B800"))
g + geom_histogram(aes(color = sex), fill = "white", alpha = 0.4, position = "identity") +
    scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
    scale_color_manual(values = c("#00AFBB", "#E7B800"))
  
#############################################################
#       HISTOGRAM + DENSITY PLOT
#############################################################
# Histogram with density plot
g + geom_histogram(aes(y = ..density..), colour = "black", fill = "White") +
    geom_density(alpha = 0.2, fill = "#FF6666")

# Color by groups
g + geom_histogram(aes(y = ..density.., colour = sex, fill = sex), alpha = 0.5, posision = "identity") +
    geom_density(aes(color = sex), size = 1)

#############################################################
#       FREQUENCY POLYGON
#############################################################
# Basic plot
g + geom_freqpoly(bins = 30) +
    theme_minimal()

# Change color and linetype by sex
g + geom_freqpoly(aes(color = sex, linetype = sex)) +
    scale_color_manual(values = c("#999999", "#E69F00")) +
    theme_minimal()

#############################################################
#       DOT PLOTS FOR ONE VARIABLE
#############################################################
g + geom_dotplot(aes(fill = sex))

#############################################################
#       ECDF PLOTS (empirical cumulative density function)
#############################################################
g + stat_ecdf(geom = "point")
g + stat_ecdf(gemp = "step")

#############################################################
#       QQ PLOTS
#############################################################
data("mtcars")
# Convert cyl to factor
mtcars$cyl <- as.factor(mtcars$cyl)
head(mtcars[, c("mpg", "cyl")])

p <- ggplot(mtcars, aes(sample = mpg))
# Basic plot
p + stat_qq()

# Change point shapes by groups
# Use custom color palette
p + stat_qq(aes(shape = cyl, color = cyl)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

#############################################################
#       BAR PLOTS OF COUNTS
#############################################################
data(mpg)
ggplot(mpg, aes(fl)) +
  geom_bar(fill = "steelblue") + theme_minimal()
