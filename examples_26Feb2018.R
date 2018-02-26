
### tidyr package ####

# http://tidyr.tidyverse.org/

## gather and spread in dplyr replace melt and case in reshape
## separate and unite are also awsome for pulling apart 

## can we use tidyr::gather and tidyr::spread rather than reshape2 package ####

## joins very basic coverage


######################

### set working directory
# setwd("C:/Users/William/Dropbox/00_DataScienceWorkshop")
setwd("E:/Dropbox/00_DataScienceWorkshop")
    
# load packages
library(tidyverse)
library(lubridate)

# load data
data("starwars")

# say we wanted to do a PCA on how mass changes with homeworld
starwars %>%
  dplyr::select(name, homeworld, species,mass) %>%
  spread(homeworld, mass)

# again fill the void
starwars %>%
  dplyr::select(name, homeworld, species,mass) %>%
  spread(homeworld, mass, fill=0)

# more complicated data sets 

## load bison data
cover2011 <- read_csv("bison_cover_2011.csv")
cover2017 <- read_csv("bison_cover_2017.csv")
point2017 <- read_csv("bison_point_2017.csv")

# here is one type of data
head(cover2017)
glimpse(cover2017)

# here is another, point counts
head(point2017)

# since these are in wide format we want to move them into long

# using only the point count data
#   select the data I want, gather the data together and then arrange

# list of codes to remove from species
grpLIST <- c("NVBARE", "NVROCK","NVLITT", "LN", "CRYCRUS","TURDBIS", "SD", "NVCWD<7","FUNGI")

tidy_point2017 <- point2017 %>%
  dplyr::select(-(Data), -(Site)) %>%
  tidyr::gather(key = Species,  value = n, TURDBIS:ROSACIC) %>%
  filter(!Species %in% grpLIST) %>%
  arrange(n)
  
# simple plot
ggplot(tidy_point2017, aes(Species, n, fill=Type)) +
  theme_bw(base_size=19) + xlab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_bar(stat="identity", position = position_dodge(1.2)) +
  facet_wrap(~Type)

# simply is there a difference between species abundance and type
m0 <- lm(n~Species*Type, data=tidy_point2017)
car::Anova(m0, type="III")


# we can also do this......

head(point2017)

point2017 %>% select(-(Data), -(Site)) %>% gather(key = Species,  value = n,  TURDBIS:ROSACIC) 

point2017 %>% select(-(Data), -(Site)) %>% gather(key = Species,  value = n, -Type, -Plot, -X, -Y ) 

####  now to make wide data ####

# to make wide data

#   must group and summarize by plot to get impact per species
effect_size <- tidy_point2017 %>%
  group_by(Type, Species) %>%
  summarize(count= mean(n, na.rm=T)) %>%
  spread(Type, count) %>%
  mutate(Effect_size = ((Exclosure-Control)/Control)) 

# plot
ggplot(effect_size, aes(y=Effect_size, x=Species)) + 
  theme_bw(base_size=19) + xlab("Species")  +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_bar(stat="identity", position = position_dodge(1.2))
  
###############



# get species list (n=33)
unique(tidy_point2017$Species)

# Tidy data are often requred by  R, in things like ggplot2
# Its also a really fast way of cleaning things up
# what are the characteristics of tidy data?

# 1) Each variable forms a column.
# 2) Each observation forms a row.
# 3) Each type of observational unit forms a table.

# This focus is on a single data set instead of a multiple connected data sets

# http://tidyr.tidyverse.org/articles/tidy-data.html

# http://garrettgman.github.io/tidying/

##########

# can we join data sets

# rbind, and cbind work, but sometimes you want something more 

# cover2011 and cover2017 data sets need to be joined or at least compared
#     cover2017 has environmental data assocated with it at end, very simple data in cover2011

# http://www.rpubs.com/williamsurles/293454

head(cover2017)
head(cover2011)

# make environmental data

grpLIST # remember this

envrLIST <- c("Location","LAT1", "LONG1", "Corner1", "LAT2", "LONG2", "Corner2", "ELEV", "ASPECT", "SLOPDEG")

# descriptions of the 5 sites
envr_df <- cover2017 %>%
  dplyr::select(-(CoverEstBy:`NVCWD<7`), -(Data)) %>%
  separate(Location, c("Region","Place")," - " )

unique(cover2017$Location)
unique(envr_df$Region); unique(envr_df$Place)

# make each data set long with list of species

cover2017_df <- cover2017 %>%
  gather(Species, Cover, BROPUMP:POPTREM) %>%
  mutate(Date=dmy(Date)) %>%
  dplyr::select(Plot, Date, CoverEstBy, Species, Cover) %>%
  replace_na(list(Cover=0)) %>%
  mutate(Cover = as.numeric(Cover))

#compare
glimpse(cover2017)
glimpse(cover2017_df)
  
# get the species list of n=55 species)
length(unique(cover2017_df$Species))

# do the same manipulation with the 2011 dataset
cover2011_df <- cover2011 %>%
  gather(Species, Cover, BROPUMP:ROSACLS) %>%
  mutate(Date=dmy(Date)) %>%
  dplyr::select(Plot, Date, CoverEstBy, Species, Cover) %>%
  replace_na(list(Cover=0)) %>%
  mutate(Cover = as.numeric(Cover))

# get the species list of n=33 species)
length(unique(cover2011_df$Species))

# compare
glimpse(cover2011)
glimpse(cover2011_df)

### can we bring these together - easy since they are the same datasets
glimpse(cover2011_df)
glimpse(cover2017_df)

cover_df <- rbind(cover2011_df, cover2017_df)
  

glimpse(cover_df)
# use separate to get teh plot data

cover_df %>% separate(Plot, c("Year","Site","Plot"), "-")

# make real because don't want to double up the names

# rbind(cover2011_df, cover2017_df) # old way

cover_df <- cover2011_df %>%
  bind_rows(cover2017_df) %>%
  separate(Plot, c("Year","Site","Plot"), "-", remove=FALSE) %>%
  filter(Site != "NA") # removes outlier


# and we can bring them back together
cover_df %>% unite("Plot", c("Year","Site","Plot"), sep = "_", remove=FALSE)


# create compatable environmental data.frame
envr_df2 <- envr_df %>% separate(Plot, c("Year","Site","Plot"), "-")

# have same codes
unique(envr_df2$Site)
unique(cover_df$Site)

all_df <- inner_join(cover_df, envr_df2, by=c("Site","Plot"))

# some messyness so might want to trim before join, some don't matter for the join

envr_df2 <- envr_df %>%
  separate(Plot, c("Year","Site","Plot"), "-") %>%
  dplyr::select(-(Year),-(Date))

# redo, and arrange to looks nice  ---- could also use rename() in pipe to clean up column names
all_df <- inner_join(cover_df, envr_df2, by=c("Site","Plot")) %>%
  dplyr::select(Date:Plot,Region:SLOPDEG,CoverEstBy, Species, Cover)

all_df



# many types of join, select function and F1 to get help, also: 

#  http://www.rpubs.com/williamsurles/293454

# filtering joins 
#       semi_jion or anti_join
#       gets rid of data that don't match
# mutating joins
#       left_join, right_join, inner_join, full_join
#       link two data sets to make a new one
#       juggle what is kept


