library(tidyverse)
library(gganimate)
library(gapminder)

#Q1. Why does it make sense to have a log10 scale on x axis?
#Answer: A possible explanation could be that the difference between low and high gpdPercap contries is very big, thus making the data hard to grasp and the visualization useless. It reduces the actual values to a more manageable size.
  

#Q2. What country is the richest in 1952 (far right on x axis)? 

gapminder %>%
  filter(year==1952) %>%
  group_by(country, gdpPercap) %>%
  arrange(desc(gdpPercap))

#The richest country in 1952 is Kuwait 

#Q3. Can you differentiate the continents by color and fix the axis labels?"

ggplot(subset(gapminder, year == 2007), aes(gdpPercap, lifeExp, color = continent , size = pop)) +
  geom_point() +
  scale_x_log10() +
  labs(x = "Gross domestic product Per Capita", y = "Life Expentacny")

# By writing color = contint i can differentiate the continents by color.

#Q4. What are the five richest countries in the world in 2007?

gapminder %>%
  filter(year==2007) %>%
  group_by(country, gdpPercap) %>%
  arrange(desc(gdpPercap))

#The five richest countries in 2007 is: Norway, Kuwait, Singapore, United States and Ireland 


#Q5 Can you add a title to one or both of the animations above that will change in sync with the animation? 

anim2 <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent, size = pop)) +
  geom_point() +
  scale_x_log10() + # convert x to log scale
  transition_time(year)
anim2

jakob1 <- anim2 + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  view_follow(fixed_y = TRUE)

jakob1

#Q6 Can you made the axes' labels and units more readable? Consider expanding the abreviated lables as well as the scientific notation in the legend and x axis to whole numbers.[hint:search disabling scientific notation]
# I don't know how to answer this question

anim2 + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  shadow_mark(alpha = 0.3, size = 0.5)

#This commands makes the units labels more readable by not switching between numerical and log10 values. 


#Q7 Come up with a question you want to answer using the gapminder data and write it down. Then, create a data visualisation that answers the question and explain how your visualization answers the question. (Example: you wish to see what was mean life expectancy across the continents in the year you were born versus your parents' birth years)
#What is the GDP development for each seperate continent? 

anim2 + facet_wrap(~continent) +
  transition_time(year) +
  labs(title = "Year: {frame_time}")

# I've used this website for inspiration and help with coding the animations: https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/