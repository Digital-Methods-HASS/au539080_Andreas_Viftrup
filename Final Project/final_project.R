#Installing and importing packages 

library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("ggthemes")
library(ggthemes)

#Please make sure that the "read.csv" matches the directory where the data set is stored on your computer. 

non_voter_data <- read.csv("Data/nonvoters_data-csv.csv")
View(non_voter_data)

#Overview over respondents. 

non_voter_data %>% 
  count(ppage) %>% 
  mutate(percent = n / sum(n) * 100) 


non_voter_data %>% 
  count(gender)

non_voter_data %>% 
  count(race) %>% 
  mutate(percent = n / sum(n) * 100)

#Plotting histogram of age distribution 

non_voter_data %>% 
  ggplot(aes(x =ppage)) +
  geom_histogram(breaks=seq(20, 100, by=5), 
                 col = "red", 
                 fill= "green", 
                 alpha = 0.2) +
  labs(title="Age Distribution of Respondents", subtitle = "Total: 5239 Respondents", x = "Age", y = "Count") +
  theme_hc() +
  theme(axis.title = element_text())


#Examining race and presidential candidate planning to support

non_voter_data %>% 
  count(race) %>% 
  mutate(percent = n / sum(n) * 100)

#Creating a dataframe that examines race and presidential candidate of respondents 
#Using the filter function to select Question 23(Which presidential candidate are you planning to support?) and filtering out respondents, who did not answer. 
#Using the mutate function to create a new variable. Grouping by race. 

race_president <- non_voter_data %>% 
  filter(Q23 != -1) %>% 
  mutate(President = as.factor(as.character(Q23))) %>% 
  count(race, President) %>% 
  group_by(race) %>% 
  mutate(percent = n / sum(n) * 100) %>% 
  ungroup()

#Renaming the variables to their corresponding presidential candidate by using the levels function. 

levels(race_president$President)[1] <- "Donald Trump"
levels(race_president$President)[2] <- "Joe Biden"
levels(race_president$President)[3] <- "Unsure"

#Plotting the bar chart

race_president %>% 
  ggplot(aes(x = race, y = percent, fill = President )) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Which Presidential Candidate Are the Different Ethnic Groups of the US Planning to Support?", subtitle = "US 2020 Presidential Election",
       x = "Race",
       y = "Percent") +
  theme_hc() +
  theme(axis.title = element_text())

#Examining relation between race and income

race_income <- non_voter_data %>% 
  mutate(Income = as.factor(as.character(income_cat))) %>% 
  count(race, Income) %>% 
  group_by(race) %>% 
  mutate(percent = n /sum(n) * 100) %>% 
  ungroup()

race_income %>% 
  ggplot(aes(x = race, y = percent, fill = Income)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Income Distribution Relative to Race",subtitle = "Year 2020",
       x = "Race",
       y = "Percent") +
  theme_hc() +
  theme(axis.title = element_text())

#Examining Voting Frequency relative to age, race, income and education

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
  labs(title ="How Voting Frequency Changes by Age", subtitle = "Total: 5239 Respondents",
       x = "Age",
       y = "Percent") +
  theme_hc() +
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
  labs(title = "Voting Frequency Relative to Race", subtitle = "Total: 5239 Respondents",
       x = "Race",
       y = "Precent") +
  theme_hc() +
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
  labs(title = "Voting Frequency Relative to Income", subtitle = "Total: 5239 Respondents",
       x = "Income",
       y = "Percent") +
  theme_hc() +
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
  labs(title = "Voting Frequency Relative to Level of Education",subtitle = "Total: 5239 Respondents", 
       x = "Level of education",
       y = "Percent") +
  theme_hc() +
  theme(axis.title = element_text())
