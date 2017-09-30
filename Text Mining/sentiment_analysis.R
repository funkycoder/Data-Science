##########################################################################
#                       Mon 25th Sept, 2017
# Coming back from 2 weeks in Belfast Northenn Ireland
##########################################################################

library(tidytext)
library(dplyr)
sentiments
glimpse(sentiments)

# Now have the data available
library(janeaustenr)
library(stringr)

tidy_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>% 
  ungroup() %>% 
  unnest_tokens(word, text)

glimpse(tidy_books)

# What are the common joy words in Emma?
# Let's use the NRC lexicon first
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
# Get Emma
tidy_books %>% 
  filter(book == "Emma") %>% 
  inner_join(nrcjoy) %>% 
  count(word, sort = TRUE)

# Now check sentiment of Jane Austen book
library(tidyr)

jane_austen_sentiment <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(book, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Let's visualize it
library(ggplot2)
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ book, ncol = 2, scales = "free_x")


# Comparing the three sentiment dictionaries
price_prejudice <- tidy_books %>% 
  filter (book == "Pride & Prejudice")
price_prejudice

afinn <- price_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  price_prejudice %>%
    inner_join(get_sentiments(("bing"))) %>%
    mutate(method = "Bing et al."),
  price_prejudice %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c(
                   "positive", "negative"
                 ))) %>%
    mutate(method = "NRC")
) %>%
  count(method, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
bing_and_nrc

# Now let's bind them then visualize
bind_rows(afinn, bing_and_nrc) %>% 
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ method, ncol = 1, scales = "free_y")

# How many positive and negative words are there in those lexicons
get_sentiments("nrc") %>% 
  filter(sentiment %in%(c("positive", "negative"))) %>% 
  count(sentiment)
get_sentiments("bing") %>% 
  count(sentiment)
# Most common positive and negative words
bing_word_counts <- tidy_books %>% 
  inner_join(get_sentiments(("bing"))) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()
bing_word_counts

# Visualize it
bing_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

# You can add custom stop words
custom_stop_words <- bind_rows(data_frame(word = c("miss"), lexicon = c("custom")), stop_words)
custom_stop_words

# Word clouds
library(wordcloud)
tidy_books %>% 
  anti_join(custom_stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))
