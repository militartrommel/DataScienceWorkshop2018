
# MULTIPLE WAYS TO CREATE A HISTOGRAM

library(tidyverse)

# read in data
steve_df <- read_csv("Quadra_Chilko_Dets.csv")

# check data
head(steve_df) 
glimpse(steve_df)


# DI_datetime_ir is a character vector, use lubridate package to adjust

library(lubridate) # http://lubridate.tidyverse.org/


# parse out the date and times NOTE: no second indicator on ymd_hm
#     done old school way NOT in pipe
steve_df$time <- ymd_hm(steve_df$DI_datetime_ir)

# the simplest way to do the histogram
ggplot(data=steve_df, aes(time)) +
  theme_bw(base_size=18) +
  geom_histogram()


# looks to see how many locations/detections per fish
#   new schools stuff
# does some data wrangling to make new data.frame and then uses this to plot it
# change label=FALSE to true to change labels but then sorting issues
plot_df <- steve_df %>%
  mutate(month_day=paste(month(time, label=FALSE),mday(time), sep="_")) %>%
  group_by(month_day) %>%
  summarize(count=n()) %>%
  filter(month_day!="NA_NA")

# plot NOTE: the x variable needs to be the same as group_by variable above
#   could use week, year, or some other period (defined in lubridate)
ggplot(plot_df, aes(x=month_day, y=count)) +
  theme_bw(base_size=18) +
  geom_bar(stat="identity")



# you can also plot over time using ggplot
#   http://ggplot2.tidyverse.org/reference/scale_date.html
ggplot(steve_df, aes(x=time, y=Tag_Burden, col=as.factor(age), shape=as.factor(age))) +
  theme_bw(base_size=18) + xlab("") +
  geom_point(size=4) +
  scale_x_datetime(date_breaks = "1 week")



 