### Code for R for Data Science Workshop

## Jan 2018


library(tidyverse)  # load tidyverse packages

data(mtcars) # load dataset

head(mtcars) # look at data

mtcars # all data

# the old way to look at models
fit <- lm(mpg~wt, data=mtcars) # fit the linear model
summary(fit) # get the summary

# we can use a new library to clean this up
library(broom)

# the new way to look at models
fit <- lm(mpg~wt, data=mtcars) # fit the model exactly the same way
tidy(fit) # clean up the model fit object
glance(fit) # take a look at it



# A more realistic example: split a data frame into pieces, fit a
#   model to each piece, summarise and extract R^2
# This uses a pipe from dplyr to make the code easier to read


mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared")

# the output is the r.squared values for the each of the different
#   cylinder types (4 cyl, 5 cyl, and 8 cyl)
unique(mtcars$cyl)

#  We can also use this process to store the model outputs in a dataframe

# If each element of the output is a data frame, use
# map_df to row-bind them together:
foo <- mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map_df(~ as.data.frame(t(as.matrix(coef(.)))))

foo # to look at output


# but there is an easier way to do this that is much easier to read
#     and gives us a little more output

library(broom)

foo2 <- mtcars %>% group_by(cyl) %>% do(tidy(lm(mpg ~ wt-1, .)))

foo2 # read the output as a tibble

# this can then be easily plotted
ggplot(foo2, aes(as.factor(cyl),estimate, fill=term)) +
  theme_bw() +
  xlab("Number of cylinders") + ylab("Estimated fuel economy (mpg with SE)") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.2, position=position_dodge(.9)) +
  geom_bar(stat="identity",position=position_dodge())


#####################################################################################


# as a function
  

red.plot <- function(x,y,...){
  plot(as.factor(x),y, ...) 
}
  
red.plot(foo2$cyl,foo2$p.value, beside=TRUE, xlab="Number of cyl", ylab="Significance level")



