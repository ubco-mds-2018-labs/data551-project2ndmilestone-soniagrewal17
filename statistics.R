#Summary"
library(tidyverse)

crimes <- read.csv("crimedata.csv", header = T)

# Number of crimes by city (in descending order)
 count(crimes, town, sort = TRUE)

# Number of crimes by category (in descending order)
 count(crimes, category, sort = TRUE)

# Frequency and percent of crime category (in descending order)
count(crimes, category, sort = TRUE) %>%
  mutate(percent = round(n/sum(n)*100, 1))

# Frequency of crime category by town(in descending order)
crimes %>%
  group_by(category,town) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) 

# Mean frequency of crime category per borough
crimes %>%
  group_by(town, category) %>%
  summarise(total = n()) %>% 
  group_by(category) %>% 
  summarise(average = round(mean(total, na.rm=TRUE), 0))

# Frequency and percent of Vehicle crime by town (in descending order)
crimes %>% 
  filter(category == "Vehicle crime") %>% 
  group_by(town) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(percent = round(n/sum(n)*100, 1))
