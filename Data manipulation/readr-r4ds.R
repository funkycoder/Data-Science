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
#Q2-common argument----
# What are the most important arguments to read_fwf()? The most important
# argument to read_fwf which reads “fixed-width formats”, is col_positions
#Q4-quote-----
x <- "x,y\n1,'a,b'"
read_delim(x, ",", quote = "'")

# Parsing a vector ----
str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(c("1", "2", "3")))
str(parse_date(c("2010-01-01", "1988-10-14")))
parse_number("$1000")
parse_number("20%")
parse_number("It cost me 200 USD")
# grouping mark
# Used in America
parse_number("$123,456,667")
# Used in many contries in Europe
parse_number("123.456.788")
parse_number("123.456.788", locale = locale(grouping_mark = "."))
# Used in SWitzerland
parse_number("123'455'666", locale = locale(grouping_mark = "'"))

# Strings ----
charToRaw("Quan Nguyen")
# Encoding
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))

# Factors ----
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "banananana"), levels = fruit)

# Problems ----
challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)

challenge <- read_csv(readr_example("challenge.csv"),
                      col_types = cols(x = col_double(),
                                       y = col_date()))
