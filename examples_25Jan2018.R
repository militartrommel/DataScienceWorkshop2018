

#### Hinch R Data Science workshop ####

# Day 2, introduction to the tidyverse
# 25 Jan 2018

# dplyr is the single most downloaded package in R
# makes more readable code
# uses a standard simple set of instructions to do powerfull things, you can do this over and over
# makes good use of dataframes as a common analysis format
# keeps you working on vertorized solutions - uses the power of R to do cool stuff

# why dataframes?  What is a tribble?

# are there other data structures than dataframes? Look up long and wide format

# what does the genetic data look like.... its wide.

######################

# tidyverse - here are the major packages, there are also many others associated with the tidyverse

library(tidyverse)

# dplyr - consistent methods for data manipulation
# ggplot - declaritive graphics
# readr - read data into R from the wild

# tibble - a differnt type of dataframe
# tidyr - helps clean data, 
# purrr - working with functions and vectors, get rid of loops
# forcats - fixes things with factors

# lubridate - dates

######################

# What does dplyr do?
# provides nice clean 4 verbs 

vignette("dplyr")

# filter() - get observations you want
# mutate() - create new variables
# select () - add or subtract variables
# arrange() - sort
# group_by() - make groups to avoid loops
# summarise() - summarize data by group

# also dplyr (along with magrittr) allows piping. This puts code in a format
#   that is more readable.  Piple use %>% to say 'then do this'. IN the example below....

# take starwars dataframe; use mutate to add a new column called foo; filter the dataframe to get only row
#   where foo == Droid; select the variables name, eye_colour, and homewold.
starwars %>%
  mutate(.,foo=species) %>%
  filter(.,foo=="Droid") %>%
  select(name,eye_color, homeworld)


# example with piping, straight off dplyr page

starwars # print dataframe
unique(starwars$species) # give lise of unique species

# get only the droids
starwars %>% 
  filter(species == "Droid")

# select particular columns
starwars %>% 
  select(name, ends_with("color"))

# create new variables
starwars %>% 
  mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name:mass, bmi)

# summarize grouped data
starwars %>%
  group_by(species) %>%
  select(-(gender)) %>%
  summarise(mean.height = mean(height),  mass = mean(mass, na.rm = TRUE)) %>%
  filter(n > 1)

## remember you can always name the output of the pipe.

## group_by is esspecially useful, it creates a series of sections in the data that summarize will then work
##    over and over on. For example, get the mean height by species as above.

##############################################################################

# here is another example using Adam's telemetry data

# modified from
#   http://varianceexplained.org/r/tidy-genomics-broom/

# set the working directory
setwd("C:/Users/William/Dropbox/00_DataScienceWorkshop")

# read in libraries
library(tidyverse)

# get data
bt <- read_csv("bltr.detections.csv") # read in using tidyverse
head(bt)

# provides counts of the number of locations for each fish, simply prints to screen
bt %>%
  mutate(fish.id = capture) %>%
  select(-capture) %>%
  group_by(fish.id) %>%
  summarize(n())

# create new clean_df dataframe that has some renamed columns and is grouped by fish.id
#   what other dply command whould you use to rename the "capture" variable to  "fish.id"
# I use arrange to sort data at end by fish.id and date.time
clean_df <- bt %>%
  mutate(fish.id = capture) %>%
  select(-capture) %>%
  group_by(fish.id) %>%
  arrange(fish.id, date.time)

# take clean_df, filter to get only two fish, then use ggplot to plot
#   often I make these speartate commands, to make it clear which is data
#   maniputlation and what is plotting but this works well to adn reduces
#   the number of named items
clean_df %>%
  filter(fish.id == "F075" | fish.id == "F104") %>%
  ggplot(aes(st.east, st.north, colour = season)) +
  theme_bw(base_size = 18) +
  geom_point(size =3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~fish.id)
  

# do the same plot above but wrap it into a function

#     first write function to plot the data
plot_telemetry_data <- function(telemetry_data) {
  ggplot(telemetry_data, aes(st.east, st.north, colour = season)) +
  theme_bw(base_size = 18) +
  geom_point(size =3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~fish.id)
  }

# then add to pipe
clean_df %>%
  filter(fish.id == "F075" | fish.id == "F104") %>%
  plot_telemetry_data()

# to plot all the fish, name the plot and save it somewhere if needed
#   note: I left it all on one line here since it is so simple,
#         how would you do this without a pipe??

#   ** warning - takes a long time **

plot_telemetry_by_season <- clean_df %>% plot_telemetry_data()




