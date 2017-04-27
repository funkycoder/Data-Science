###########################################################
#             BABY NAMES IN THE US                        #
# Original author : VIT TRAN                              #
# Date : April 26, 2017 (Wednesday)
# url: http://www.rpubs.com/tkvit/228141                  #
###########################################################
# Load libraries
library(tidyverse)
library(xts)
library(dygraphs)
# or
pkg <- c("tidyverse",
         "xts",
         "dygraphs"
)
lapply(pkg, require, character.only = TRUE)

rm(list = ls())
library(tidyverse)
df <- read.csv("Data visualization/data/NationalNames.csv",
               # Sử dụng header:
               header = TRUE)

top5_name <- function(df, gender) {
  (
    df %>%
      filter(Gender == gender) %>%
      group_by(Name) %>%
      summarise(name_count = sum(Count),
                num_years = n_distinct(Year)) %>%
      ungroup() %>%
      arrange(desc(name_count)) %>%
      slice(1:5)
  )
}

# Top 5 names in the dataset 
(m.top5 <- top5_name(df, "M"))
(f.top5 <- top5_name(df, "F"))

# Top 1 name is recently popular or long time ago?
top1_years <- function(top_dat) {
  # This shows that function doesnt have to return a value
  df %>%
    filter(Name == top_dat$Name[1]) %>%
    group_by(Year) %>%
    summarise(n_count = sum(Count)) %>%
    ggplot(aes(x = Year, y = n_count)) +  geom_bar(stat = "identity", color = "#3399FF") +
    theme_bw() + ggtitle(as.character(top_dat$Name[1]))
}
# Plot distribution by year for top1 name for male and female
top1_years(m.top5)
top1_years(f.top5)

# Top name since 2010
(
  df %>%
    filter(Year > 2010) %>%
    group_by(Name) %>%
    summarise(n_count = sum(Count)) %>%
    arrange(desc(n_count)) %>%
    slice(1:10)
)

# Name havent been used since 2010
(
  df %>% 
    group_by(Name) %>% 
    summarise(n_count = sum(Count),
              first_use = min(Year),
              last_use = max(Year)) %>% 
    filter(last_use < 2010) %>% 
    arrange(desc(n_count)) %>% 
    slice(1:10)
)
###################################
# Dygraph most variant names ------
###################################
variance <- df %>% 
  group_by(Name) %>% 
  summarise(sd = sd(Count))%>%
  na.omit() %>% 
  filter(sd != 0) %>% 
  filter (sd > mean(sd)) %>% 
  ungroup() %>% 
  arrange(desc(sd)) %>% slice(1:5)

quelnoms <- df %>% filter(Name %in% variance$Name) %>% 
  select(Name,Year,Count)

quelnoms1 <- quelnoms %>% group_by(Name,Year) %>% 
  summarise(count = sum(Count))  %>% 
  mutate(Year = ymd(paste0(Year,"0101")))

# Spread the data
quelnoms1 <- spread(quelnoms1, Name, count) 

#convert to time series data class
timeserie <-xts(quelnoms1, order.by = quelnoms1$Year)

#dygraph
dygraph(timeserie) %>% 
  dyRangeSelector() %>%
  dyHighlight(highlightCircleSize = 4, 
              highlightSeriesBackgroundAlpha = 0.5,
              hideOnMouseOut = TRUE)%>%
  dyLegend(show = "follow")
