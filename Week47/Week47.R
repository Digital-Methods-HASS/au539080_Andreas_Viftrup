#1)
room <- c(1, 2, 1, 3, 1, NA, 3, 1, 3, 2, 1, NA, 1, 8, 3, 1, 4, NA, 1, 3, 1, 2, 1, 7, 1, NA)

#Removing NA values
room[!is.na(room)]

#Defining a new vector without the NA values 
roomsort <- room[!is.na(room)]

#Displaying values higher than 2 
roomsort [roomsort>2]

#2)

mean(roomsort)
round(2,318282)

#3) 

str(room)
#It's a numerical vector. R recognizes NA as a number 

#4

install.packages("tidyverse")
interviews <- read_csv("data/SAFI_clean.csv", na = "NULL")



