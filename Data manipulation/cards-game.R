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

# heart game ----
deck_heart <- deck
deck_heart$value <- 0
head(deck_heart, 13)
# assign value of 1 to every card that has a suit of hearts
deck_heart$value[deck_heart$suit == "hearts"] <- 1
deck_heart$value[deck_heart$suit == "hearts"]

queenOfSpades <- deck_heart$face == "queen" & deck_heart$suit == "spades"
deck_heart[queenOfSpades, ]

deck_heart$value[queenOfSpades] <- 13

# blackjack game ----
blackjack <- deck
head(blackjack, 13)

face_card <- blackjack$face %in% c("king", "queen", "jack")
blackjack$value[face_card] <- 10
head(blackjack, 13)

blackjack$value[blackjack$face == "ace"] <- NA
head(blackjack, 13)

# ENVIRONMENTS ----
# deal 3 times to have the same card
for (i in 1:3) {
  print(deal(deck))
}


