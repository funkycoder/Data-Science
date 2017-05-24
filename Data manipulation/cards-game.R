library(readr)
deck <- read_csv("data/deck.csv", col_names = TRUE)

# deal function ----
# deal the first card
deal <- function(cards) {
  return(cards[1, ])
}

# shuffle function ----
shuffle <- function(cards) {
  random <- sample(1:52, size = 52)
  return(cards[random, ])
}
