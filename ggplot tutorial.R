####Why use ggplot?####
#Consistent grammar
#flexible
#theme system for easily ensuring consistency among figures
#many users = more online help!
#More verbose for simple graphics but less so for more complex graphics

####Ggplot combines various plot building blocks####
#data**
#aesthetic mapping (aes)**
#geometric object (boxplot, hist, points, etc.)**
#statistical transformations
#scales
#coordinate system
#position adjustments
#faceting (panels)**

###I USE R COOKBOOK FOR GRAPHS FOR NEARLY EVERY GGPLOT FIGURE I MAKE!!###
#http://www.cookbook-r.com/Graphs/

#load required packages
require(ggplot2)

####how does ggplot work????####
#sarting simple with summary data
meal.dat <- data.frame(
  sex = factor(c("Female","Female","Male","Male")),
  time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(13.53, 16.81, 16.24, 17.42)
) # a dataframe with only 4 observations of 3 variables

#make a bar graph of bill amounts by eating time
ggplot(meal.dat, aes(x=time, y=total_bill))
#ggplot will not assume what kind of figure you need! Need to define plot type.

# Stacked bar graph (probably not what you want)
windows()
ggplot(meal.dat, aes(x=time, y=total_bill, fill=sex)) + #data
  geom_bar(stat="identity") #what kind of graph you want
#default is stat count, which cannot be used with a y aes. Stat identity shows the data.

# Use position_dodge() to have bars side by side
ggplot(meal.dat, aes(x=time, y=total_bill, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge())

# Easy to change groupings and add outline
ggplot(meal.dat, aes(x=sex, y=total_bill, fill=time)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")

#A finished graph:
# A bar graph
ggplot(meal.dat, aes(x=time, y=total_bill, fill=sex)) + 
  geom_bar(colour="black", stat="identity",
           position=position_dodge(),
           size=.3) +                        # Thinner lines
  scale_fill_discrete(name="Sex of payer") +      # Set legend title
  xlab("Time of day") + ylab("Total bill") + # Set axis labels
  ggtitle("Average bill for 2 people") +     # Set title
  theme_bw() +
  scale_y_continuous(limits = c(0,20), expand = c(0, 0)) # I dislike the blank space below the bars

#Two plot types
# Basic line graph with points
plot1<- ggplot(meal.dat, aes(x=time, y=total_bill, group=sex)) 
plot1
plot1 +
  geom_line() + #we want lines
  geom_point() #and we want points

# We want to colour the lines and points by group
ggplot(meal.dat, aes(x=time, y=total_bill, group=sex, colour=sex)) + #you can add colours to all plot types
  geom_line() + geom_point()

# We want to colour both lines and points by groups of different aesthetic
ggplot(meal.dat, aes(x=time, y=total_bill, group=sex)) +
  geom_line(aes(colour=sex)) + geom_point() #or add another aesthetic

#Each grouping has different shape and colour (often redundant)
ggplot(meal.dat, aes(x=time, y=total_bill, group=sex, shape=sex, colour=sex)) +
  geom_line(size=1.5) +  #change sizes
  geom_point(size=5)+
  scale_shape_manual(values=c(18,8)) #overide default shape selection


# Could also map sex to different shapes and line types through additions of aesthetics 
# Make points and lines larger with a fill
ggplot(meal.dat, aes(x=time, y=total_bill, group=sex)) +
  geom_line(aes(linetype= sex), size=2) +
  geom_point(aes(shape=sex), size=6, colour="red", fill="green")+
  scale_shape_manual(values=c(22,21)) + #overide default shape selection (google ggplot shapes)
  scale_linetype_manual(values=c(2,3)) #override default line type
#with fill colour selected in shape, need shapes that can be filled
#can also extend lines in legend (more in legends)

# A finished graph
ggplot(meal.dat, aes(x=time, y=total_bill, group=sex, shape=sex, colour=sex)) + 
  geom_line(aes(linetype=sex), size=1) +     # Set linetype by sex
  geom_point(size=3, fill="white") +         # Use larger points, fill with white
  scale_colour_hue(name="Sex of payer",      # Set legend title
                   l=30)  +                  # Use hue instead of manual to make colours darker but not change them (l=lightness)
  scale_shape_manual(name="Sex of payer",
                     values=c(22,21)) +      # Use points with a fill color
  scale_linetype_discrete(name="Sex of payer") +  #need to change legens of all aesthetics!
  xlab("Time of day") + ylab("Total bill") + # Set axis labels
  ggtitle("Average bill for 2 people") +     # Set title
  theme_bw()                                 # Set BW theme

# Often are datasets are not just of simple summary stats!
# Though R Cookbook has a code for a summarizing function ("summary_SE") which will break down your data
# Could also be done in a pipe
# Best to view all data

######Histograms####### 
data<-mtcars
ggplot(data, aes(x=hp)) + geom_histogram() #default is to have 30 bins

# Specify bin width and draw with black outline, white fill
ggplot(data, aes(x=hp)) +
  geom_histogram(binwidth=10, colour="black", fill="white")

#Or specify number of bins
ggplot(data, aes(x=hp)) +
  geom_histogram(bins=10, colour="black", fill="white")

#add vertical line for mean
ggplot(data, aes(x=hp)) +
  geom_histogram(binwidth=10, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(hp)),  #add vertical line
             color="red", linetype="dashed", size=1)

# I sometimes prefer density curves for showing data
ggplot(data, aes(x=hp)) + geom_density() #they can look nicer than this!!

#they're especially informative when comparing distributions
ggplot(data, aes(x=hp, group=cyl, fill=factor(cyl))) + 
  geom_density(alpha=0.1) #alpha makes the colours transparent

#You can do this with standard historgrams as well though 
ggplot(data, aes(x=hp, fill=factor(cyl))) +
  geom_histogram(binwidth=10, alpha=.5, position="identity")  #identity plots data as it is

# Interleaved histograms
ggplot(data, aes(x=hp, fill=factor(cyl))) +
  geom_histogram(binwidth=10, position="dodge") #position dodge plots bars side by side

#this isn't very pretty if range of data differs a lot between groupings
ggplot(data, aes(x=hp, fill=factor(cyl))) + geom_histogram(binwidth=10, colour="black") + 
  facet_wrap(~ cyl) + #sometimes better to facet
  guides(fill = FALSE) # no need for legend when using colours

#Another way to view distributions...box plots
# A basic box plot
ggplot(data, aes(x=factor(cyl), y=mpg)) + geom_boxplot()

# A basic box with the conditions colored
ggplot(data, aes(x=factor(cyl), y=mpg, fill = cyl)) + #shows range of colours because cyl is numeric
    geom_point(position=position_jitter(width=0.1)) +
  geom_boxplot()

# A basic box with the conditions colored
ggplot(data, aes(x=factor(cyl), y=mpg)) + #cyl factored here but not in dataset
  geom_boxplot(aes(fill=factor(cyl))) #could add another aesthetic

####scatterplots####
#and more complex data

#load data
data("midwest", package = "ggplot2")
#http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
midwest <- read.csv("http://goo.gl/G1K41K")

#turn off exponential notation
options(scipen=999)

#you can also use pipes with ggplot
midwest%>%
  ggplot(aes(x=area, y=poptotal)) +
  geom_point()

#add line
midwest%>%
  ggplot(aes(x=area, y=poptotal)) +
  geom_point() + geom_line() #geom line that we used before connect points, often not useful

#need geom smooth to find patterns in points 
midwest%>%
  ggplot(aes(x=area, y=poptotal)) +
  geom_point() + geom_smooth(method=lm, se=T) #can set se to F
#default is loess but aslo available:  "lm", "glm", "gam", "loess", "rlm" 


#want to see variability in data for smaller populations? (e.g. copy number in microbe data)
#make your plot an object to add things to
plot <- midwest %>%
  ggplot(aes(x=area, y=poptotal)) + 
  geom_point() + geom_smooth(method=lm, se=F)
plot
#select desired y-axis "limit"
plot + xlim(c(0, 0.1)) + ylim(c(0, 500000)) #This deletes points! (see errors)
#Essentailly a data exclusion function and changes linear component accordingly
plot + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 500000)) #does not delete points (no error)
#does not change regression, just zooms in
#see original plot
plot

#One way to add titles and labels
plot +
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", 
       x="Area", caption="Midwest Demographics")

#play with colour and characeristics!
midwest%>%
  filter(state=="IL")%>% #benefits of piping
  ggplot(aes(x=area, y=poptotal)) + 
  geom_point(col="blue", size=3, shape=3) +   # Set color and size for points
  geom_smooth(method="lm", col="red", lty="dashed") +  # change characteristics of line
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 500000)) + #zoom in
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", 
       x="Area", caption="Midwest Demographics")

#set colour by another factor
midwest%>%
  ggplot(aes(x=area, y=poptotal)) + #aes for data
  geom_point(aes(shape=state, colour=as.factor(inmetro)), size=2) +  #new aes for points
  geom_smooth(method="lm", col="red", size=1) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 500000)) + #zoom in
  labs(title="Area Vs Population", subtitle="From midwest dataset", 
       y="Population", x="Area", caption="Midwest Demographics")
#legends can all be re-named


####Making graphs pretty####
#jittering points
#important when there are lots of points on top of each other
ggplot(data, aes(x=cyl, y=mpg)) +
  geom_point()
#better jittered
windows()
ggplot(data, aes(x=cyl, y=mpg, colour=factor(cyl))) +
  geom_point(aes(group=vs, shape=factor(vs)),      # Use hollow circles
             position=position_dodge(0.8)) 

#titles and labels
#generic boxplot
box<- ggplot(data, aes(x=factor(cyl), y=mpg)) + #cyl factored here but not in dataset
  geom_boxplot(aes(fill=factor(cyl))) + #could add another aesthetic
  theme_bw()
box
box + ggtitle("MPG and No. of Cylinders") + ylab("Miles per gallon") + xlab("Cylinders")
## Equivalent to
box + labs(title="MPG and No. of Cylinders", y="Miles per gallon", x="Cylinders")

# If the title is long, it can be split into multiple lines with \n
box + ggtitle("Obviously miles per gallon is higher with fewer cylinders and this title is unnecessarily long")
box + ggtitle("Obviously miles per gallon is higher with fewer cylinders\n and this title is unnecessarily long")

#update "box" to include labels and titles
box<- box + labs(title="MPG and No. of Cylinders", y="Miles per gallon", x="Cylinders")

# Manually set the order of a discrete-valued axis
box + scale_x_discrete(limits=c("6","4","8"))

# Manually name discrete axis values
box + scale_x_discrete(labels=c("Four", "Six", "Eight"))

# Hide x tick marks, labels, and grid lines (not needed with legend)
box + scale_x_discrete(breaks=NULL)

# Specify tick locations manually (continuous vars)
box + scale_y_continuous(breaks=seq(10, 36, 2))  # Ticks from 10-36, every 2

# The breaks can be spaced unevenly
box + scale_y_continuous(breaks=c(10, 12, 15, 25, 27, 29, 32, 40))

# Suppress ticks and gridlines
box + scale_y_continuous(breaks=NULL)

# Instead of applying a blanket "NULL" to breaks, you can do many things with theme
box + theme(axis.ticks = element_blank(), axis.text.x = element_blank())

#BUT if theme is being used with a preset theme it has to go afterwards
#this removes ticks and axis labels
ggplot(data, aes(x=factor(cyl), y=mpg)) +
  geom_boxplot(aes(fill=factor(cyl))) + 
  theme_bw() + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
#this does not
ggplot(data, aes(x=factor(cyl), y=mpg)) +
  geom_boxplot(aes(fill=factor(cyl))) + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  theme_bw()

# In this case, we don't need an x-axis label and a legend
#remove legends can be done in many ways
#remove for a particular aesthetic (fill)
box + guides(fill=FALSE)
# It can also be done when specifying the scale
box + scale_fill_discrete(guide=FALSE)
# Or in theme
box + theme(legend.position="none")
#I find it easiest to do everything in theme

#You can modify parts of the legend
# Remove title for fill legend (better to do it this way if you have multiple aesthetics)
box + guides(fill=guide_legend(title=NULL))

# Remove title for all legends
box + theme(legend.title=element_blank())

#change location of legend
box + theme(legend.position="top")

# Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
box + theme(legend.position=c(0.9,0.8))

#use scale_fill_discrete to change aspects of legend
#title
box + scale_x_discrete(breaks=NULL, name="") + #keeping legend, don't need x-axis labels
  scale_fill_discrete(name="No. of\nCylinders") #name legend
#change order and labels
box + scale_x_discrete(breaks=NULL, name="") + 
  scale_fill_discrete(name="No. of\nCylinders",
                         breaks=c("6", "8", "4"),
                         labels=c("Six", "Eight", "Four"))
#note the "scale_x_discrete changes order on x-axis, not legend

# With scale fill manual you can also manually adjust colours
box + scale_x_discrete(breaks=NULL, name="") + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                        name="No. of\nCylinders",
                         labels=c("Four", "Six", "Eight")) #don't need breaks because keeping order

#In a line graph would use scale_colour_xxx and/or scale_shape_xxx instead of scale_fill_xxx. colour maps to the colors of lines and points, while fill maps to the color of area fills. shape maps to the shapes of points.
#I.e., aes set to colour or shape, not fill and aligns with discrete or continuous nture of data


#If you use two aesthetics, both need to be given scale specifications. 
# Simple meal.dat line graph to show how to deal with 2 legends
# Specify colour and shape
line <- ggplot(meal.dat, aes(x=time, y=total_bill, group=sex, shape=sex, colour=sex)) + 
  geom_line() + geom_point()
line
#Otherwise there will be two two separate legends.
line + scale_colour_discrete(name  ="Payer",
                            breaks=c("Female", "Male"),
                            labels=c("Woman", "Man"))
# Specify both colour and shape
line + scale_colour_discrete(name  ="Payer",
                            breaks=c("Female", "Male"),
                            labels=c("Woman", "Man")) +
  scale_shape_discrete(name  ="Payer",
                       breaks=c("Female", "Male"),
                       labels=c("Woman", "Man"))


######FACETS#####
facet.plot<- ggplot(midwest, aes(x=area, y=poptotal)) +
  geom_point() + geom_smooth(method="lm", se=F) +
  theme_bw()
facet.plot
# Divide by state in the horizontal direction
facet.plot + facet_grid (state ~ .)
# Vertical
facet.plot + facet_grid (. ~ state)
# With wrap you can specify number of columns
facet.plot + facet_wrap( ~ state, nrow=2)
# You can also grid by two factors
facet.plot + facet_grid(state ~ inmetro)

# Don't have to kee the same scales for all rows of plots
facet.plot + facet_grid(inmetro ~ state, scales="free_y")

#change characteristics of facet lebls with theme
facet.plot + facet_grid(state ~ inmetro) +
  theme(strip.text.x = element_text(size=8, angle=75),
        strip.text.y = element_text(size=12, face="bold"),
        strip.background = element_rect(colour="red", fill="#CCCCFF"))
#change text of facets with labeller within facet function
facet.plot + facet_grid(. ~ inmetro, labeller=labeller(inmetro = c( "0" = "Rural", "1" = "Urban")))


#Multiple plots together
#either multiplot function from R cookbook or gridarrange in tiff function (saving high resolution images)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

#use mult plot function
multiplot(box, facet.plot, line, plot, cols=2)


#Some formatted examples from recent papers
#1) Faceted boxplots with annoted text and lines added
tiff("Figure3.tiff", width = 6, height = 3, units = 'in', res = 1500)
ggplot(sex.mat.fig, aes(x=interaction(mat.cat, time.point), y=inj)) + 
  geom_boxplot(aes(fill=mat.cat), colour = "black", size=0.2, outlier.size = 0.2) + 
  facet_wrap(~Sex)+
  theme(strip.background=element_rect(fill="black", 
                                      strip.text=element_text(color="white", 
                                                              face="bold", size=12)),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA), 
        panel.grid.major.y = element_line("light grey", size = 0.1), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  labs(x=" ", y="Injury Score") +
  geom_vline(xintercept=3.5, color="black", size=0.5) +
  scale_y_continuous(limits=c(4.8,8.95), breaks=seq(4.8,8.95, by = 0.5)) +
  annotate("text", x = 2, y = 8.95, label = "Day 0") +
  annotate("text", x = 5, y = 8.95, label = "Day 5") +
  scale_fill_grey(start = 0.4, end = .9, name="Maturity\nCategory",
                  labels=c("Silver", "Silver Bright", "Mature"))
dev.off()

#2) predicted probabilities of each reflex impairment category given net handling conditions
tiff("Figure4.tiff", width = 6, height = 3, units = 'in', res = 1500) #write tiff
ggplot(A6predN, aes(x=time.pursed, y=pred.prob, color=RAMP.score)) +  #plot predicted probabilties (from logistic regression) with time pursed by RAMP score
  geom_point(aes(shape = RAMP.score), size=1.5) +                     #use different shapes by RAMP score
  geom_smooth(method=lm, se=T, size=0.5,                              #linear regression
              aes(linetype = RAMP.score), alpha=0.2) +                #linetype by RAMP score, lighten colour of SE 
  facet_grid(. ~ crowd,                                               #facet by if fish were crowded or not
             labeller=labeller(crowd = c( "0" = "Not Crowded", "1" = "Crowded"))) +   #label facets
  labs(x = "Time Pursed in Net (min.)", y = "Predicted Probability") + #label axes
  scale_colour_brewer(palette="Paired", name="No. Reflexes\nImpaired", #change text in labels and colour scheme of both lines and points
                      labels=c("0", "1", "2", "3", "4", "5", "6")) +
  xlim(4, 43) +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0, 0.4, 0.05)) +    #expand limits to remove gap and add custom breaks
  theme(panel.background = element_blank(),                           #change various parts of theme
        strip.background = element_rect(fill="black"),
        panel.border = element_rect(colour = "black", fill=NA), 
        panel.grid.major.y = element_line("light grey", size = 0.1), 
        strip.text=element_text(color="white", face="bold", size=12), 
        legend.key.width=unit(2, "cm"))+
  scale_linetype_manual(values=c("twodash", "twodash", "solid", "solid", "dashed", "dashed"), 
                        name="No. Reflexes\nImpaired",               #change linetypes manually and change legend accordingly
                        labels=c("0", "1", "2", "3", "4", "5", "6"))+
  scale_shape_manual(values=c(8, 8, 17, 17, 16, 16), name="No. Reflexes\nImpaired", 
                     labels=c("0", "1", "2", "3", "4", "5", "6"))    #change shape types manually and change legend accordingly
dev.off()  #need dev.off to save tiff file


#2) Linear Discriminant Analyses figures
inj.lda.values <- read.csv("inj.lda.value.csv")
p1<-ggplot(inj.lda.values, aes(x=x.LD1, fill=as.factor(class))) + geom_density(alpha=.7) +
  ylab("Density") + theme_bw() +
  xlab("Linear Discriminant 1 Values") +
  scale_fill_manual(values=cbPalette, name="Injury Category",
                    labels=c("Uninjured", "Minor", "Moderate", "Severe")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-3.4, 4.9)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.position=c(0.03,0.9), legend.justification=c(0.03,1), 
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Divergence Among Injury Categories")
p2<-ggplot(lda.dat, aes(x=as.factor(injury1), y=Factor1, fill=as.factor(injury1))) + 
  geom_boxplot(alpha = .7) + guides(fill=FALSE) + 
  scale_fill_manual(values =  cbPalette) +
  ylab("Ion Homeostasis Score") + theme_bw() + xlab(" ") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(expand = c(0, 0), limits = c(-1.6, 1.5))
p3<-ggplot(lda.dat, aes(x=as.factor(injury1), y=FL, fill=as.factor(injury1))) + 
  geom_boxplot(alpha = .7) + guides(fill=FALSE) + 
  scale_fill_manual(values =  cbPalette) +
  ylab("Fork Length (mm)") + theme_bw() + xlab(" ") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous(breaks=seq(425, 725, 50))
setwd("C:/Users/Katrina Cook/Google Drive/Hinch Lab/Publications/Coho holding studies")
##Create TIFF
tiff('inj.lda.tiff', units="in", width=8, height=6, res=600)
grid.arrange(p1,                             # First row with one plot spaning over 2 columns
             arrangeGrob(p2, p3, ncol = 2), # Second row with 2 plots in 2 different columns
             nrow = 2)                       # Number of rows
dev.off()

                