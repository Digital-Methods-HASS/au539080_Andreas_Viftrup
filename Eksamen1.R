library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("ggthemes")
library(ggthemes)

non_voter_data <- read.csv("Data/nonvoters_data-csv.csv")

#Overview over respondents

non_voter_data %>% 
  count(ppage) %>% 
  mutate(percent = n / sum(n) * 100) 


non_voter_data %>% 
  count(gender)

non_voter_data %>% 
  count(race) %>% 
  mutate(percent = n / sum(n) * 100)




#Race and president 

non_voter_data %>% 
  count(race) %>% 
  mutate(percent = n / sum(n) * 100)

race_president <- non_voter_data %>% 
  filter(Q23 != -1) %>% 
  mutate(President = as.factor(as.character(Q23))) %>% 
  count(race, President) %>% 
  group_by(race) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  ungroup()

levels(race_president$President)[1] <- "Donald Trump"
levels(race_president$President)[2] <- "Joe Biden"
levels(race_president$President)[3] <- "Unsure"

race_president %>% 
  ggplot(aes(x = race, y = percent, fill = President )) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Planning to support by race", 
       x = "Race",
       y = "Precent") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

#Voting Frequency

non_voter_data %>% 
  count(voter_category)

vote_age <- non_voter_data %>% 
  mutate(Vote = as.factor(as.character(voter_category))) %>% 
  count(ppage, Vote) %>% 
  group_by(ppage) %>% 
  mutate(percent = n /sum(n) * 100) %>% 
  ungroup()

vote_age %>% 
  ggplot(aes(x = ppage, y = percent, color = Vote)) + 
  geom_line(size = 1.5, alpha = 0.8) + 
  labs(title ="How voting frequency changes by age",
       x = "Age",
       y = "Percent") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

#Question: How do i divided age into groups?

vote_age %>% 
  ggplot(aes(x = ppage, y = percent, fill = Vote)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "How often do you vote by age",
       x = "Age",
       y = "Precent") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

#By race

vote_race <- non_voter_data %>% 
  mutate(Vote = as.factor(as.character(voter_category))) %>% 
  count(race, Vote) %>% 
  group_by(race) %>% 
  mutate(percent = n /sum(n) * 100) %>% 
  ungroup()

vote_race %>% 
  ggplot(aes(x = race, y = percent, fill = Vote)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Voting frequency by race", 
       x = "Race",
       y = "Precent") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

#By income 

vote_income <- non_voter_data %>% 
  mutate(Vote = as.factor(as.character(voter_category))) %>% 
  count(income_cat, Vote) %>% 
  group_by(income_cat) %>% 
  mutate(percent = n /sum(n) * 100) %>% 
  ungroup()

vote_income %>% 
  ggplot(aes(x = income_cat, y = percent, fill = Vote)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Voting frequency by income", 
       x = "Income",
       y = "Precent") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

#By education 

vote_educ <- non_voter_data %>% 
  mutate(Vote = as.factor(as.character(voter_category))) %>% 
  count(educ, Vote) %>% 
  group_by(educ) %>% 
  mutate(percent = n /sum(n) * 100) %>% 
  ungroup()

vote_educ %>% 
  ggplot(aes(x = educ, y = percent, fill = Vote)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Voting frequency by education", 
       x = "Level of education",
       y = "Precent") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

#Trust in the system Q8

non_voter_data %>% 
  count(Q8_1) %>% 
  filter(Q8_1 != -1)

  

  





#Can I make an animation that goes from Q8_1 to Q8_9


 

#Age of respondents

age_respondents <- non_voter_data %>% 
  count(ppage, race) %>% 
  mutate(percent = n / sum(n) * 100)
 
age_respondents %>% 
  ggplot(aes(x = ppage, y = percent)) +
  geom_line(size = 1.5, alpha = 0.8) + 
  labs(title = "Age distrubution of respondents",
       x = "Age",
       y = "Percent of respondents") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

age_respondents %>% 
  ggplot(aes(x =ppage)) +
  geom_histogram(binwidth = 1)

age_respondents %>% 
  ggplot(aes(x =ppage, y=percent)) +
  geom_histogram(binwidth = 1)


#Race and income

race_income <- non_voter_data %>% 
  mutate(Income = as.factor(as.character(income_cat))) %>% 
  count(race, Income) %>% 
  group_by(race) %>% 
  mutate(percent = n /sum(n) * 100) %>% 
  ungroup()

race_income %>% 
  ggplot(aes(x = race, y = percent, fill = Income)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Income by race",
       x = "Race",
       y = "Precent") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())


  #Test

non_voter_data %>% 
  count(Q2_1, Q2_2) 

