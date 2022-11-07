library(tidyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(lexicon) #hash_sentiment_senticnet
library(wordcloud)
library(reshape2)

#Load working data--------------------------------------------------------------
libdevtools::install_github("EmilHvitfeldt/sherlock")
library(sherlock)

holm <- holmes %>%
  group_by(book) %>%
  mutate(chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) |>
  filter(chapter != 0) |> #remove intro from all books
  ungroup()


##Define lexicons---------------------------------------------------------------
#Get sentiments (original)
afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")


tidy_books <- holmes %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) |>
  filter(chapter != 0) %>%
  ungroup() %>%
  unnest_tokens(word, text)

#Get joyful words from nrc
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")


#Get count of joyful words from Holmes series
tidy_books %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

#Get count of words for every 80-line section using bing
holmes_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

#Plot sentiment for each segment
ggplot(holmes_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

#Comparing the three sentiment dictionaries------------------------------------
#Extract one book
asib <- tidy_books %>% 
  filter(book == "A Study In Scarlet")

#Get sentiment for AFINN
afinn <- asib %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

#Get sentiment for bing and nrc
bing_and_nrc <- bind_rows(
  asib %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  asib %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)


#visualize all 3 lexicons
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

#Get count of positive vs negative words
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

##Most common positive and negative words---------------------------------------
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#visualize most common positive and negative words
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#Add a custom stop word to list
custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)


#Wordclouds---------------------------------------------------------------------
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

#Using my own lexicon-----------------------------------------------------------
#Define my chosen lexicon and rename columns
sentiword <- hash_sentiment_senticnet %>% 
  rename(
    word = x,
    sentiment_score = y
  )


#Get count of words for every 80-line section using sentiword
holmes_sentiment_sentiword <- tidy_books %>%
  inner_join(sentiword) %>%
  sum(book, index = linenumber %/% 80, sentiment_score) %>%
  mutate(sentiment = positive - negative)
