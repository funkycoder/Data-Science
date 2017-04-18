###################################################################
#                             UDEMY
#      Data Science and Machine Learning Bootcamp with R
###################################################################
# We'll be covering the following functions:
# filter() (and slice())
# arrange()
# select() (and rename())
# distinct()
# mutate() (and transmute())
# summarise()
# sample_n() and sample_frac()

library(nycflights13)
library(dplyr)

# filter() allows you to select a subset of rows in a data frame. The
# first argument is the name of the data frame. The second and subsequent
# arguments are the expressions that filter the data frame:
head(filter(flights, year == 2013, month == 3, carrier == "AA"))
# Conventional way
head(flights[flights$month == 11 & flights$day == 3 & flights$carrier == 'AA', ])

# slice()
# We can select rows by position using slice()
slice(flights, 2:20)

# arrange() works similarly to filter() except that instead of filtering or
# selecting rows, it reorders them. It takes a data frame, and a set of column
# names (or more complicated expressions) to order by. If you provide more than
# one column name, each additional column will be used to break ties in the
# values of preceding columns:
head(arrange(flights, year, month, day, arr_time))
head(arrange(flights, desc(month), day, desc(arr_time)))


# select() Often you work with large datasets with many columns but only a few
# are actually of interest to you. select() allows you to rapidly zoom in on a
# useful subset using operations that usually only work on numeric variable
# positions:
head(select(flights, year, month, day, dep_time, arr_time))

# rename() You can use rename() to rename columns, note this is not "in-place"
# you'll need to reassign the renamed data structures.
head(rename(flights,airline_car = carrier, departuretime = dep_time))


# distinct() A common use of select() is to find the values of a set of
# variables. This is particularly useful in conjunction with the distinct() verb
# which only returns the unique values in a table.
distinct(flights, carrier)

# mutate() Besides selecting sets of existing columns, it’s often useful to add
# new columns that are functions of existing columns. This is the job of
# mutate():
head(mutate(flights, new_col = arr_delay - dep_delay))

# transmute() Use transmute if you only want the new columns
head(transmute(flights, new_col = arr_delay-dep_delay))

# summarise() You can use summarise() to quickly collapse data frames into
# single rows using functions that aggregate results. Remember to use na.rm=TRUE
# to remove NA values.
summarise(flights, avg_air_time=mean(air_time,na.rm=TRUE))

# sample_n() and sample_frac() You can use sample_n() and sample_frac() to take
# a random sample of rows: use sample_n() for a fixed number and sample_frac()
# for a fixed fraction.
sample_n(flights,10)
# .005% of the data
sample_frac(flights,0.00005) # USE replace=TRUE for bootstrap sampling
