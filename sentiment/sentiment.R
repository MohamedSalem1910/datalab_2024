library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidytext) 
library(gsheet)
library(wordcloud2) 
library(sentimentr)
library(lubridate)

survey <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1W9eGIihIHppys3LZe5FNbUuaIi_tfdscIq521lidRBU/edit?usp=sharing')

#visulisation1
survey2 <- survey %>% 
  mutate(date_time = mdy_hms(Timestamp))
ggplot(data = survey2, aes(x=date_time, y=first_name)) +
         geom_col()
#visualisation2
survey2 <- survey %>% 
  mutate(date_time = mdy_hms(Timestamp))
ggplot(data = survey2, aes(x=date_time)) +
  geom_bar()
#exploring sentiment
sentiments <- get_sentiments('bing')
nrow(sentiments)
ncol(sentiments)

#words object
words <- survey %>%
  dplyr::select(first_name,
                feeling_num,
                feeling) %>%
  unnest_tokens(word, feeling)

#making a word cloud
word_freq <- words %>% 
  group_by(word) %>% 
  tally()
wordcloud2(word_freq, size = 2 )

#sw
sw <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/stopwords.csv')

#Remove from word_freq any rows in which the word appears in sw.
sw2 <- sw %>% 
  group_by(word) %>% 
  tally()
word_freq22 <- word_freq %>% 
  semi_join(word_freq, sw2, by = "word")
