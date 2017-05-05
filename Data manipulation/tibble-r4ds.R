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
