

# data for Steve Johnston on how to plot a time series from different tidal stations

# Exersise: HOW CAN YOU DO THIS WITHOUT CALL INDIVIDUAL WORKSHEETS/.csv IN EXCEL. DEVELOP
#             CODE TO READ DIRECTLY FROM THE EXCEL FILE 

# load libraries
library(tidyverse)
library(lubridate)

#setwd("C:/Users/William/Dropbox/00_DataScienceWorkshop")

setwd("E:/Dropbox/00_DataScienceWorkshop")

# get data, must have saved as csv
tidal_browns <- read_csv("browns.csv")
tidal_whaletown <- read_csv("whaletown.csv")

## CAN YOU READ IN THE DATA DIRECTLY FROM WORKSHEETS ##

# read_excel() # read in the excel file
# excel_sheets() # gets the name of the excel sheets
# read_excel(xlsx_example, sheet = "chickwts")


# read in edited browns worksheet
#     COULD USE TIDY::UNITE HERE ALSO
browns_df <- tidal_browns %>%
  gather(time, height, -(Date)) %>%
  mutate(date_time_chr = paste(Date, time, sep=" ")) %>%
  mutate(date_time = ymd_h(date_time_chr)) %>%
  mutate(Station = "Browns Bay") %>%
  dplyr::select(Station, date_time, height)
browns_df

# basic plot
ggplot(browns_df, aes(x=date_time, y=height, shape=Station, colour=Station)) +
  theme_bw(base_size=18) + ylab("Tide height in meters") + xlab("") +
  geom_point(size=1, pch=21) +
  geom_line()

# add in the next worksheet, without any edits in the worksheet
whaletown_df <- tidal_whaletown %>%
  gather(time, height, -(PDT)) %>%
  mutate(date_time_chr = paste(PDT, time, sep=" ")) %>%
  mutate(date_time = ymd_h(date_time_chr)) %>%
  mutate(Station = "Whaletown Bay") %>%
  dplyr::select(Station, date_time, height)
whaletown_df

# join two data sets, must have exact same columns
tidal_df <- rbind(browns_df, whaletown_df)

# another plot, note identical becasue written in general form
ggplot(tidal_df, aes(x=date_time, y=height, shape=Station, colour=Station)) +
  theme_bw(base_size=18) + ylab("Tide height in meters") + xlab("") +
  geom_point(size=1, pch=21) +
  geom_line()

# with facets
ggplot(tidal_df, aes(x=date_time, y=height, shape=Station, colour=Station)) +
  theme_bw(base_size=18) + ylab("Tide height in meters") + xlab("") +
  geom_point(size=1, pch=21) +
  geom_line() +
  facet_wrap(~Station)


