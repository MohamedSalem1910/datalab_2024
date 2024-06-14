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
sw2<- sw
wordfreq_sw = anti_join(word_freq, sw2)

#new wordcloud

wordcloud2(wordfreq_sw, size = 2 )

#Make an object with the top 10 words used only. Name this object top10
wordfreq_sw <- wordfreq_sw %>% 
  arrange(desc(wordfreq_sw$n))
top10 <- head(wordfreq_sw, 10) 

#Create a bar chart showing the number of times the top10 words were used.
ggplot(data = top10, aes(x=word, y=n)) +
  geom_col()

#20.Run the below to join word_freq with sentiments
wordfrq_sentiments <- word_freq %>% 
  left_join(sentiments, by = "word")
#22.For the whole survey, were there more negative or positive sentiment words used?
wordfrq_sentiments %>% 
  drop_na() %>% 
  group_by(sentiment) %>%
  tally()
#23. Create an object with the number of negative and positive words used for each person.
sent_frq <- wordfrq_sentiments %>%
  drop_na() %>% 
  group_by(sentiment) %>%
  tally()
#24. In that object, create a new variabled named sentimentality, which is the number of positive words minus the number of negative words.
sent_frq <- sent_frq %>% 
  mutate("sentimentality" = 40-25)
# 25. Make a histogram of senitmentality
ggplot(data = sent_frq, aes(x= sentimentality)) + 
  geom_histogram()
# 26. Make a barplot of sentimentality.
ggplot(data = sent_frq, aes(x= sentimentality)) + 
  geom_bar()
# 27. Create a wordcloud for the dream variable.

# 28. Create a barplot showing the top 16 words in our dreams.
# 
# 29. Which word showed up most in people’s description of Joe?
#   
#   30. Make a histogram of feeling_num.
# 
# 31. Make a density chart of feeling_num.
# 
# 32. Change the above plot to facet it by gender.
# 
# 33. How many people mentioned Ryan Gosling in their description of Joe?
#   
#   34. Is there a correlation between the sentimentality of people’s feeling and their feeling_num?