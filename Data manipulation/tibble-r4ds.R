#############################################################
#                       TIBBLE                              #
#############################################################
# Load libraries
library(tidyverse)
library(nycflights13)

# More info ----
package?tibble
vignette("tibble")
# Use tibble as data.frame ----
as_tibble(iris)

# Nonsyntactic names ----
# Surround them by backtick
tb <- tibble(`:)` = "smile",
             ` ` = "space",
             `2000` = "number")
tb

# tribble (transposed tibble) ----
tribble(
  ~x, ~y, ~z,
  #--/---/---/
  "a", 2, 3.6,
  "b", 1, 8.6
)

# tibble vs data.frame ----
# Show only first 10 rows
# Columns fit on screen only
# Each column report its type
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

# tibble print ----
# display all columns
# You can control print behavior with setting options
nycflights13::flights %>% 
  print(n = 10, width = Inf)
nycflights13::flights %>% 
  View()

# Subsetting ----
df <-  tibble(
  x = runif(5),
  y = rnorm(5)
)
# Extract by name
df$x
df[["x"]]

# Use with pipe
df %>% .$x

# Convert to data.frame
class(as.data.frame(df))

# Exercise tibble ----
# Q1- tibble ----
# How can you tell if an object is a tibble? (Hint: try printing mtcars, which is a regular data frame).
mtcars
class(mtcars)
as_tibble(mtcars)
# Tibbles will only print out a limited number of rows and show the class on top
# of each column. Addtionally, tibbles have class "tbl_df" and "tbl" in
# addition to "data.frame".
class(as_tibble(mtcars))

# Q2- tibble Compare and Contrast----- 
# Compare and contrast the following operations on a data.frame and
# equivalent tibble. What is different? Why might the default data frame
# behaviours cause you frustration?
df <- data.frame(abc = 1, xyz = "a")
# Partial matching
df$x
# Result is a factor
class(df$x)
df[, "xyz"]
class(df[, "xyz"])
df[, c("abc", "xyz")]
# Result is a data.frame
class(df[, c("abc", "xyz")])

tbl <- as_tibble(df)
# No partial matching
tbl$x
tbl[, "xyz"]
tbl[, c("abc", "xyz")]
# With data.frames, with [ the type of object that is returned differs on the
# number of columns. If it is one column, it won’t return a data.frame, but
# instead will return a vector. With more than one column, then it will return a
# data.frame. This is fine if you know what you are passing in, but suppose you
# did df[ , vars] where vars was a variable. Then you what that code does
# depends on length(vars) and you’d have to write code to account for those
# situations or risk bugs.

# Q3-tibble Var ----
# If you have the name of a variable stored in an object, e.g. var <- "mpg", how
# can you extract the reference variable from a tibble? You can use the double
# bracket, like df[[var]]. You cannot use the dollar sign, becuase df$var would
# look for a column named var.

# Q4- tibble Nonsyntactic name ----
# Practice referring to non-syntactic names in the following data frame by:
# Extracting the variable called 1.
# Plotting a scatterplot of 1 vs 2.
# Creating a new column called 3 which is 2 divided by 1.
# Renaming the columns to one, two and three.
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
annoying

# Extract the variable called 1:
annoying[[1]]
annoying$`1`

# A scatterplot of 1 vs. 2:
ggplot(annoying, aes(x = `1`, y = `2`)) + geom_point()

# A new column 3 with is 2 divided by 1:
annoying[["3"]] <- annoying$`2` / annoying$`1`
# or
annoying[["3"]] <- annoying[["2"]] / annoying[["1"]]

# Renaming the columns to one, two, and three:
annoying <- rename(annoying, one = `1`, two = `2`, three = `3`)
glimpse(annoying)

#Q5-tibble::enframe ----
# What does tibble::enframe() do? When might you use it?
# It converts named vectors to a data frame with names and values
?tibble::enframe
enframe(c(a = 1, b = 2, c = 3), name = "var_name", value = "var_value")

#Q6-tibble print options ----
# What option controls how many additional column names are printed at the footer of a tibble?
# The print function for tibbles is in print.tbl_df:
  ?print.tbl_df
# The option n_extra determines the number of extra columns to print information for.