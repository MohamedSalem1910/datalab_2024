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
  filter(name %in% c('Joe', 'Marie', 'Mary', 'John', 'Johnny', 'Jack' ))
ggplot(data = names2002, aes(x = name, y= n)) +
  geom_col(fill = "blue")



#Create a new data set called the_nineties that only contains years from the 1990s.

the_nineties <- bb_names %>% 
  filter(year < 2000) %>% 
  filter(year > 1989)

#Save this dataset to your repository (use write_csv()).
write_csv(the_nineties, "the_nineties.csv")

fifties <- babynames %>% 
  filter(year < 1960) %>% 
  filter(year > 1949) %>% 
  filter(sex == "F")

#Now that everything is up to date, make a visualisation of you and your team member’s names for a year of your choice.
groupnames <- bb_names %>% 
  filter(year == 2003) %>% 
  filter(name %in% c('Ramzy', 'Oysik', 'Mohamed', 'Buchanan'))
ggplot(data = groupnames , aes(x = name, y= n, fill = sex)) +
  geom_col()
