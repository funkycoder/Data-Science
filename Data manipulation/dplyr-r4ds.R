###########################################################################
#                   R FOR DATA SCIENCE                                    #
#                       dplyr()                                           #     
###########################################################################
# filter
#-------
library(dplyr)
library(nycflights13)
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

# arrange()
#----------
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

# mutate()
-------------
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

# Offsets
# -----------
# shift vector value to the right
(x <- 1:10)
lag(x)
# Shift vector value to the left
lead(x)
# Cummulative aggregates
cumsum(x)
cummean(x)

# Ranking
# -------------
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
