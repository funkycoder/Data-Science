# readr ----
# read_csv : Comma-delimited
# read_csv2: semicolon 
# read_tsv: tab-delimited
# read_delim: any delimeter

library(tidyverse)
heights <- read_csv("data/heights.csv")

# inline csv ----
read_csv("a, b, c
         1, 2, 3
         4, 5, 6")
read_csv("the first line of metadata
        the second line of metadata
        x,y,z
        1,2,3",
        skip = 2)
read_csv("# A comment I want to skip
         x,y,z
         1,2,3",
         comment = "#")
# data don't have column name
read_csv("1,2,3\n4,5,6", col_names = FALSE)
# specify a column name
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))
# NA values
read_csv("a,b,c\n1,2,3\n4,5,.", na = ".")

# Exercise readr----
#Q1-readr-sep"|" ----
read_delim("a|b|c\n1|2|3\n4|5|6", delim = "|")
#Q2-common argument
# What are the most important arguments to read_fwf()? The most important
# argument to read_fwf which reads “fixed-width formats”, is col_positions
#Q4-quote
x <- "x,y\n1,'a,b'"
read_delim(x, ",", quote = "'")
