# Load library
library(tidyverse)
# Take a look at a few table
table1
table2
table3
table4a
table4b
# Remember dplyr and ggplot2 are designed to work with tidy data
# Compute rate per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)
# Cases per year
table1 %>% 
  count(year, wt = cases)
# Visualize change over time
library(ggplot2)
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))


# Exercise 1 ----
# table_2
tbl2_case <- filter(table2, type == "cases")[["count"]]
tbl2_country <- filter(table2, type == "cases")[["country"]]
tbl2_population <- filter(table2, type == "population")[["count"]]
tbl2_year <- filter(table2, type == "cases")[["year"]]
tbl2_clean <- tibble(country = tbl2_country, year = tbl2_year, rate = tbl2_case / tbl2_population)
tbl2_clean
# table4a and table4b
tibble(country = table4a[["country"]],
       `1999` = table4a[["1999"]] / table4b[["1999"]],
       `2000` = table4a[["2000"]] / table4b[["2000"]])
# or
tibble(country = rep(table4a[["country"]], 2),
       year = rep(c(1999, 2000), nrow(table4a)),
       rate = c(table4a[["1999"]] / table4b[["1999"]], table4a[["2000"]] / table4b[["2000"]])
       )
# Recreate the plot showing change in cases over time using table2 instead of
# table1. What do you need to do first? First, I needed to filter the tibble to
# only include those rows that represented the “cases” variable.
table2 %>% 
  filter(type == "cases") %>% 
    ggplot(aes(year, count)) +
      geom_line(aes(group = country), color = "grey50") +
      geom_point(aes(color = country))

# Gathering ----
table4a
tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
table4b
tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)

# Spreading ----
table2
spread(table2, key = "type", value = "count")

# Exercise 2 ----
stocks <- tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c(1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks
stock1 <- stocks %>% 
  spread(year, return)
stock1 %>% 
gather(`2015`:`2016`, key = "year", value = "return")
