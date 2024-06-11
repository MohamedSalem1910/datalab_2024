library(babynames)
library(ggplot2)
library(dplyr)
library(tidyverse)
bb_names <- babynames

#Create a histogram of the name Marie in 1982.

Marie1982 <- bb_names %>% 
  filter(name == "Marie") %>% 
  filter(year <= "1982")


ggplot(data = Marie1982, aes(x=year, y=n, color =sex )) +
  geom_col()


#Create a line plot for proportion of the name Joe, colored by sex. Make the lines a bit thicker and more transparent

Joes <- bb_names %>% 
  filter(name == "Joe")
ggplot(data = Joes, aes(x = year, y= n, color = sex)) +
  geom_line(size = 2, alpha = 0.5) +
  labs(x= "Year", y= "Number", title = "The number of Joes over the years")


#Create a bar chart of all female names in 2002

names2002 <- bb_names %>% 
  filter(year == 2002) %>% 
  filter(name %in% c('Joe', 'Marie', 'Mary', 'John', 'Johnny', ))
ggplot(data = names2002, aes(x = name, y= n)) +
  geom_bar()
