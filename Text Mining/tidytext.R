##########################################################################
#                       Wed 06th Sept, 2017
# Long time coming back to R, is it that difficult for you to stick with?
##########################################################################

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just ourselves -",
          "and Immortality")
library(dplyr)
text_df <- data_frame(line = 1:4, text = text)

# Convert to: one-token-per-document-per-row
library(tidytext)
text_df %>% 
  unnest_tokens(word, text)

#---------------------------------
# TIDYING THE WORKS OF JANE AUSTEN
#---------------------------------
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>% 
  ungroup()

# Convert to one-token-per-document-per-row
library(tidytext)
tidy_books <- original_books %>% 
  unnest_tokens(word,text)

tidy_books

# Remove stop words
# Use all 3 lexicons
data(stop_words)
tidy_books <- tidy_books %>% 
  anti_join(stop_words)

# Find the most common words in all the books
tidy_books %>% 
  count(word, sort = TRUE)

# Visualization
library(ggplot2)
tidy_books %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 600) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

##########################################
#   Gutenbergr package
##########################################

library(gutenbergr)
hgwells <- gutenberg_download(c(35, 36, 5230, 159))
tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_hgwells %>% 
  count(word, sort = TRUE)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_bronte %>% 
  count(word, sort = TRUE)

# Binding the data frames together
library(tidyr)
frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sister"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z]+")) %>% 
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Bronte Sister` : `H.G. Wells`)

# Visualization
library(scales)
ggplot(frequency, aes(
  x = proportion,
  y = `Jane Austen`,
  color = abs(`Jane Austen` - proportion)
)) +
  geom_abline(color = "gray40", lty =  2) +
  geom_jitter(
    alpha = 0.1,
    size = 2.5,
    width = 0.3,
    height = 0.3
  ) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0001),
                       low = "darkslategray4",
                       high = "gray75") +
  facet_wrap(~ author, ncol = 2) +
  theme(legend.position = "none") +
  labs(y = "Jane Austen", x = NULL)

# Now let's quantify how similar and different these set or word frequencies
cor.test(data = frequency[frequency$author == "Bronte Sister",], ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "H.G. Wells",], ~ proportion + `Jane Austen`)

# Obviously Austen and Bronte have more correlated words than Austen and Wells