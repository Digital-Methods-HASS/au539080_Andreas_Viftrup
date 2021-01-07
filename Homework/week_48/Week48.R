

library(tidyverse)
library(gganimate)
library(gapminder)

#Look at the data 

unique(gapminder$year)
head(gapminder)

ggplot(subset(gapminder, year == 1952), aes(gdpPercap, lifeExp, size = pop)) +
  geom_point() +
  scale_x_log10() 

#Q1. Why does it makes sense to have a log10 scale on x axis? 
#Answer: A possible explanation could be that the difference between low and high gpdPercap contries is very big,
#thus making the data hard to grasp and the visualization useless. It reduces the actual values to a more manageable size.

#Q2. What country is the richest in 1952 (far right on the x axis)?

gapminder %>%
  filter(year==1952) %>%
  group_by(country, gdpPercap) %>%
  arrange(desc(gdpPercap))

#Q3. Can you differentiate the continents by color and fix the axis labels?

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





## Make it move 
### Option 1: Anmite using transition_states(x)

install.packages("gganimate")
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")
## "C:\\rtools40\\usr\\bin\\make.exe"
install.packages("jsonlite", type = "source")
install.packages("ggplot2")
library(ggplot2)
library(gganimate)

```{r anim1 start
anim <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop)) +
  geom_point() +
  cale_x_log10()
anim
