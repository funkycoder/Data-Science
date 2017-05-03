#---- Start -----
###########################################################################
#                   R FOR DATA SCIENCE                                    #
#                       dplyr()                                           #     
###########################################################################
# Load necessary libraries
library(dplyr)
library(nycflights13)
library(ggplot2)

#---- filter-----
str(flights)
# All flights had an arrival delay of two or more hours
filter(flights, arr_delay >= 120)

# Flew to  Houston
filter(flights, dest == "IAH" | dest == "HOU")
filter(flights, dest %in% c("IAH", "HOU"))

# Operated by United, American, Delta
# Carrier code for airliners
distinct(flights, carrier)
filter(flights, carrier %in% c("UA", "AA", "DL"))

# Departed in summer (July, August, september)
filter(flights, month %in% c(7, 8, 9))
filter(flights, between(month, 7, 9))

# Arrived more than two hours late but didn't leave late
filter(flights, arr_delay >= 120 & dep_delay <= 0)

# Delayed by at least an hour but made up over 30mins in flight
filter(flights, dep_delay >= 60 & arr_delay <= 30)

# Departed between midnight and 6am (inclusive)
filter(flights, dep_time == 2400 | between(dep_time, 1, 600))

# How many flights missing dep_time?
count(filter(flights, is.na(dep_time)))

#---- arrange()----
# Sort missing value to the start/ default is at the bottom
arrange(flights, desc(is.na(dep_delay)))

# Most delayed flights
arrange(flights, desc(dep_delay))

# Left earliest
arrange(flights, dep_time)

# Fastest flights
View(arrange(flights, air_time))

# select()
#----------
# Select all columns between year and day (inclusive)
select(flights, year:day)

# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

# rename()
# variant of select (keeps all the var that aren't
# explicitly mentioned
rename(flights, tail_num = tailnum)

# everything() helper
# If you have some variables that you want to move to the start of df
select(flights, time_hour, air_time, everything())

# Select dep_time, dep_delay, arr_time, arr_delay
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights,
       -(year:day),
       -sched_dep_time,
       -sched_arr_time,
       -(carrier:time_hour))
select(flights, dep_time:arr_delay, -sched_dep_time,-sched_arr_time)

# one_of()
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

# select helpers and case situation (ignore.case = TRUE)
select(flights, contains("TIME"))

select(flights, one_of(c("year", "month")))

#---- mutate()------
# Create new column at THE END of your data frame
flights_sm <- select(flights,
                     year:day, 
                     ends_with("delay"), 
                     distance, 
                     air_time)
mutate(flights_sm,
       gain = arr_delay - dep_delay,
       speed = distance/ air_time * 60)
# You can refer to the column you've created
mutate(flights_sm,
      gain = arr_delay - dep_delay,
      hours = air_time * 60,
      gain_per_hour = gain / hours
)
# HEY mutate doesn't change the original data frame
head(flights_sm)

# If you only want to keep the new variable then use transmute
transmute(flights_sm,
       gain = arr_delay - dep_delay,
       hours = air_time * 60,
       gain_per_hour = gain / hours
)

################################################
#         USEFUL CREATION FUNCTIONS
################################################
# Compute hour and minute from dep_time
# dep_time : 157 means 1:57
transmute(flights,
          dep_time,
          dep_time %/% 100,
          dep_time %% 100)

# Offsets ----
# shift vector value to the right
(x <- 1:10)
lag(x)
# Shift vector value to the left
lead(x)
# Cummulative aggregates
cumsum(x)
cummean(x)

# Ranking ----
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
# sort largest first
min_rank(desc(y))

# Convert dep_time and sched_dep_time 
# to number of minutes since midnight
transmute(
  flights,
  dep_time_minute = dep_time %/% 100 * 60 + dep_time %% 100,
  sched_dep_time_minute = sched_dep_time %/% 100 * 60 + sched_dep_time %% 100
)
# Compare air_time and arr_time - dep_time
transmute(
  flights,
  air_time = as.integer(air_time),
  arr_time,
  dep_time,
  flight_time = arr_time - dep_time
)
# Convert to minutes from midnight to accurately calculate the flight time
transmute(
  flights,
  air_time = as.integer(air_time),
  arr_time,
  arr_time_minute = arr_time %/% 100 * 60 + arr_time %% 100,
  dep_time,
  dep_time_minute = dep_time %/% 100 * 60 + dep_time %% 100,
  flight_time = (arr_time_minute - dep_time_minute) %/% 60 * 100 + (arr_time_minute - dep_time_minute) %% 60
)

# Take a look at dep_time, sched_dep_time, dep_delay
head(
  transmute(
    flights,
    dep_time,
    sched_dep_time,
    dep_delay = as.integer(dep_delay),
    difference = sched_dep_time - dep_time
  ),
  100
)

# Top 10 delayed flights
head(transmute(flights, delay_rank = min_rank(desc(dep_delay)), tailnum, time_hour), 10)
# Test dense_rank like min_rank, but with no gaps between ranks
head(transmute(flights, delay_rank = dense_rank(desc(dep_delay)), tailnum, time_hour), 10)

#---- Group Summaries -----
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

#Average delay by date
flights %>% 
  group_by(year, month, day) %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE))

#----- Pipe -------
delay <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1 / 3) +
  geom_smooth(se = FALSE)

# ---- Missing Values ------
# What happens if we dont use na.rm?
flights %>%
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))
# Now you can remove missing values (cancel flights)
flights %>%
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay, na.rm = TRUE))
# You can first filter out the cancelled flights
not_cancelled <- flights %>%
  filter(!is.na(dep_delay),!is.na(arr_delay))
# then calculate as usual
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))

# ---- Counts -----
# Look at planes that have the highest average delays
not_cancelled %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay)) %>%
  ggplot(mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

# Draw a scatter plot of number of flights vs average delaydelay
not_cancelled %>%
  group_by(tailnum) %>%
  summarise(delay = mean(arr_delay),
            n = n()) %>%
  ggplot(mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1 / 10)
# So you see that the variation decreases as the sample size increases
not_cancelled %>%
  group_by(tailnum) %>%
  summarise(delay = mean(arr_delay),
            n = n()) %>%
  filter(n > 25) %>%
  ggplot(mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1 / 10)

# Using the Lahman package
batting <- as_tibble(Lahman::Batting)
batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE)/ sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )
batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x = ab, y = ba)) +
    geom_point() +
    geom_smooth(se = FALSE)
#If you naively sort on desc(ba), the people with the best batting average are
#clearly lucky not skilled
batters %>% 
  arrange(desc(ba))

#######################
# Useful summary functions -----
not_cancelled <- flights %>%
  filter(!is.na(dep_delay),!is.na(arr_delay))
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    # average delay
    avg_delay1 = mean(arr_delay),
    # average positive delay
    avg_delay2 = mean(arr_delay[arr_delay > 0])
  )

# Why is distance to some destinations are more variable than to others?
not_cancelled %>% 
  group_by(dest) %>% 
  summarize(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

# Measures of min(x), quantile(x, 0.25), max(x)
# When do first and last flights leave each day
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

# Measures of position first(x), nth(x, 2), last(x)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )
# Filter on rank
not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  select(year:day, tailnum, dep_time, r) %>% 
  filter(r %in% range(r))
# Counts
# Which destinations have the most carriers
not_cancelled %>% 
  group_by(dest) %>% 
  summarize(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))
# Quick counts
not_cancelled %>% 
  count(dest)
# weight variable
not_cancelled %>% 
  count(tailnum, wt = distance)

# How many flights left before 5am? these usually indicate delayed flights from 
# the previous day
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))
# What proportion of flights are delayed more than an hour
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(hour_perc = mean(arr_delay > 60))

####################
# Grouping by multiple variables ----
daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarize(per_month, flights =sum(flights)))

head(daily)
daily %>% 
  ungroup() %>% 
  summarise(flighs = n())
# Group mutates (and filters) ----
# Find the worst member of each group
flights_sm %>% 
  group_by(year, month, day) %>% 
  filter(rank(desc(arr_delay)) < 10)

# All groups bigger than a threshold
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests

# Standardize to compute per group metrics
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

# Exercise ----
# Another approach
not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(n())

not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(sum(distance))
# Cancelled flights per day
daily_cancelled <- flights %>%
  group_by(year, month, day) %>%
  filter(is.na(dep_delay), is.na(arr_delay)) %>%
  summarise(flight_cancelled = n())
g <- ggplot(daily_cancelled, aes(x = flight_cancelled))
g + geom_histogram(bins = 100) +
    geom_vline(aes(xintercept = mean(flight_cancelled), color = "#FC4E07"),
             linetype = "dashed",
             size = 1)
# Proportion of cancelled flight vs average delay
dat <- flights %>%
  group_by(year, month, day) %>%
  summarise(cancelled_perc = mean(is.na(arr_delay)),
            avg_delay = mean(arr_delay, na.rm = TRUE))
ggplot(dat, mapping= aes(x = avg_delay, y = cancelled_perc * 100)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Worst delay carriers
flights %>% 
  group_by(carrier, dest) %>% 
  summarize(Count = n(), avg_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  # Get the max delay only with frequent flight (>50 flight)
  filter(Count > 50, avg_delay == max(avg_delay)) %>% 
  arrange(desc(avg_delay))

# Average delay by carrier
flights %>% 
  group_by(carrier) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(avg_delay))
# Average delay by destination
flights %>%
  group_by(dest) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(avg_delay))

# Ex-Group and Mutate Filter ----

# Q2-Group and Mutate Filter ---- 
# Which plane have the worst on-time record
flights %>%
  group_by(tailnum) %>% 
  summarise(mean_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  # ungroup() %>%
  filter(rank(desc(mean_arr_delay)) <= 1)

# Q3-Group and Mutate Filter ---- 
# What time of day should you fly if you want to avoid delays as much as
# possible Let’s group by hour. The earlier the better to fly. This is intuitive
# as delays early in the morning are likely to propogate throughout the day.
flights %>%
  group_by(hour) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(avg_delay)

flights %>% 
  group_by(hour) %>% 
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE))

# Q4-Group and Mutate Filter ---- 
# For each destination, compute the total minutes of delay. 
# For each, flight,compute the proportion of the total delay for its destination.
flights %>% 
  count(dest, wt = arr_delay, sort = TRUE)

flights %>%
  filter(!is.na(arr_delay), arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(total_delay = sum(arr_delay),
         prop_delay = arr_delay / total_delay) %>% 
  select(tailnum, dest, total_delay, prop_delay)

# Alternatively, consider the delay as relative to the minimum delay for any
# flight to that destination. Now all non-cancelled flights have a proportion.
flights %>% 
  filter(!is.na(arr_delay), arr_delay > 0) %>%  
  group_by(dest) %>%
  mutate(total_delay = sum(arr_delay - min(arr_delay)),
         prop_delay = arr_delay / sum(arr_delay))


# Q5-Group and Mutate Filter ---- 
# Delays are typically temporally correlated: even once the problem that caused
# the initial delay has been resolved, later flights are delayed to allow
# earlier flights to leave. Using lag() explore how the delay of a flight is
# related to the delay of the immediately preceding flight. We want to group by
# day to avoid taking the lag from the previous day. Also, I want to use
# departure delay, since this mechanism is relevant for departures. Also, I
# remove missing values both before and after calculating the lag delay.
# However, it would be interesting to ask the probability or average delay after
# a cancellation.

flights %>%
  group_by(year, month, day) %>%
  filter(!is.na(dep_delay)) %>%
  mutate(lag_delay = lag(dep_delay)) %>%
  filter(!is.na(lag_delay)) %>%
  ggplot(aes(x = dep_delay, y = lag_delay)) +
  geom_point() +
  geom_smooth()


# Q6-Group and Mutate Filter ---- 
# Look at each destination. Can you find flights that are suspiciously fast?
# (i.e. flights that represent a potential data entry error). Compute the air
# time a flight relative to the shortest flight to that destination. Which
# flights were most delayed in the air? 

# The shorter BOS and PHL flights that are
# 20 minutes for 30+ minutes flights seem plausible - though maybe entries of
# +/- a few minutes can easily create large changes. I assume that departure
# time has a standardized definition, but I’m not sure; if there is some
# discretion, that could create errors that are small in absolute time, but
# large in relative time for small flights. The ATL, GSP, an BNA flights looks a
# little suspicious as it’s almost half the time for longer flights.

flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest) %>%
  mutate(med_time = median(air_time),
         fast = (air_time - med_time) / med_time) %>%
  arrange(fast) %>%
  select(air_time,
         med_time,
         fast,
         dep_time,
         sched_dep_time,
         arr_time,
         sched_arr_time,
         tailnum) %>%
  head(15)

# I could also try a z-score. Though the sd and mean will be affected by large delays.
flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest) %>%
  mutate(
    air_time_mean = mean(air_time),
    air_time_sd = sd(air_time),
    z_score = (air_time - air_time_mean) / air_time_sd
  ) %>%
  arrange(z_score) %>%
  select(
    z_score,
    air_time_mean,
    air_time_sd,
    air_time,
    dep_time,
    sched_dep_time,
    arr_time,
    sched_arr_time
  )

flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest) %>%
  mutate(air_time_diff = air_time - min(air_time)) %>%
  arrange(desc(air_time_diff)) %>%
  select(dest,
         year,
         month,
         day,
         carrier,
         flight,
         air_time_diff,
         air_time,
         dep_time,
         arr_time) %>%
  head()

# Q7-Group and Mutate Filter ---- 
# THIS QUESTION IS NOT YET SOLVED!
# Find all destinations that are flown by at least two carriers.
# Use that information to rank the carriers
flights %>% 
  group_by(dest) %>% 
  count(carrier) %>% 
  group_by(carrier) %>% 
  count(sort = TRUE)
