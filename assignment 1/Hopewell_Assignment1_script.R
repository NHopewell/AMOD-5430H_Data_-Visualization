##### AMOD 5430H: Data Visualization 
##### Assignment 1
##### Nicholas Hopewell
##### 0496633


# load packages
library(ggplot2)
library(gridExtra)
library(psych)

# prints first rows of dataframe
head(CO2)


## QUESTION 4 ###

# iterating over clumns in CO2 dataframe
for (i in 1:ncol(CO2)) {
  
  # if column is numeric, do the following 
  if (class(CO2[[i]]) == "numeric") {
    # print variable name (column name) 
    print(paste0("Variable: ", colnames(CO2)[i])) # string concatentaion (similar to cat())
    # compute and print descriptive statistics, rounded to 2 decimal places
    print(paste0("Mean    : ", round(mean(CO2[[i]]), 2)))
    print(paste0("SD      : ", round(sd(CO2[[i]]), 2)))
    print(paste0("Median  : ", round(median(CO2[[i]]), 2)))
    print(paste0("Range   : ", round(range(CO2[[i]]), 2)))
    
  }
}

# check if right - function to get descriptives (psych::describe)
describe(CO2[,4:5])


# subset CO2 to include first 50 rows
ggplot(CO2[1:50, ], aes(x="", y=uptake))+ # visualizing uptake
  geom_boxplot(size = 1) + #boxplot with lines thicker than default
  ggtitle("Carbon Dioxide Uptake") + # titles
  ylab("Uptake rates (umol/m^2 sec)") +
  xlab(NULL)

# now by group 'treatment'
ggplot(CO2, aes(x= Treatment, y=uptake))+
  geom_boxplot(size = 1) + # boxplot with thicker lines
  ggtitle("Carbon Dioxide Uptake by Treatment") + # titles
  ylab("Uptake rates (umol/m^2 sec)") +
  xlab("Treatment")

# visualizing uptake
ggplot(CO2, aes(x=uptake)) +
  geom_histogram(colour="black", bins = 5) + #histogram with 5 bins
  ggtitle("Carbon Diaoxide Uptake") + # titles
  xlab("Uptake rates (umol/m^2 sec)") 

# visualizing uptake
ggplot(CO2, aes(x=uptake)) +
  geom_histogram(colour="black", bins = 10) + # histogram with 10 bins
  ggtitle("Carbon Diaoxide Uptake") + # titles
  xlab("Uptake rates (umol/m^2 sec)") 


## Question 5 ##

# visualzing model
ggplot(mpg, aes(x = model)) + 
  geom_dotplot(color = 'red', method = 'histodot', # dot plot, red outline, histogram format to remove overlapping 
               binwidth = 1, stackratio = 1.2) + # binwidth, space stacked dots slightly
  ggtitle("Counts of Vehicle Models") + # title
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # adjust axis labels to 90 degrees, anchor labels to scale line

# visualzing clas
ggplot(mpg, aes(x=class)) +
  geom_bar(colour="black") + # bar chart filled as black
  ggtitle("Class of Vehicle") + # titles
  xlab(NULL)

# visualizing manufacturer
ggplot(mpg, aes(x=manufacturer)) +
  geom_bar(colour="black") + # bar chart of colour black
  ggtitle("Representation of Car Manufacturers") + # titles
  xlab(NULL) + # no x label 
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) # angle text and fix horrizontal adjustment

# visualizing class
ggplot(mpg, aes(x=displ, fill=class)) +
  geom_histogram(colour="black") + # histogram filled black
  facet_grid(class ~ .)+ # splitting displays by class
  ggtitle("Engine Displacement by Class") + # title
  theme(legend.position="none") # no legend - not needed with facet grid

# visualizing city mpg
ggplot(mpg, aes(x=cty)) + 
  geom_density(alpha = 0.2, fill = "black") + # density plot, transparent, filled black
  ggtitle("Density Distribution of City Mpg") + # titles
  xlab("mpg")

# store first plot in object
p1 <- ggplot(mpg, aes(x=cty)) + # visualizing city mpg
  geom_density(alpha = 0.2, fill = "black") + # density plot, transparent, filled black
  ggtitle("Density Distribution of City Mpg") + # titles
  xlab("mpg")

# store second plot in object
p2 <- ggplot(mpg, aes(x=hwy)) + # visualizing city mpg
  geom_density(alpha = 0.2, fill = "black") + # density plot, transparent, filled black
  ggtitle("Density Distribution of Highway Mpg") + # titles
  xlab("mpg")

# arrange plots to be side by side (gridExtra::grid.arrage)
grid.arrange(p1, p2,
             nrow = 1) # both in one row

# visualzing quantiles for hwy miles
ggplot(mpg, aes(sample=hwy)) + 
  stat_qq() + # qqplot
  ggtitle("Distribution of Sample Vehicle Highway Mpg") + # titles
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles HWY Mpg")

# quantiles for cty miles
ggplot(mpg, aes(sample=cty)) + 
  stat_qq() + # qqplot
  ggtitle("Distribution of Sample Vehicle City Mpg") + # titles
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles CTY Mpg")

# converting Month to a factor and giving factor levels labels
# cannot simple do as.factor() later because the labels are desired. 
airquality$Month <- factor(airquality$Month,
                           labels = c("May", "Jun", "Jul", "Aug", "Sep"))

# visualizing wind speeds
ggplot(airquality, aes(x=Wind)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1, # 1 ob per bin
                 colour="black", fill="light grey") + # set colours
  geom_density(alpha=.2, fill="blue") + # overlay density, transparent, blue
  ggtitle("Wind Speeds in Miles per Hour") + # titles
  xlab("Mph") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"), # increase title size and make bold
        axis.title = element_text(face="bold")) # make axis title bold

# visualizing temp
ggplot(airquality, aes(x="", y=Temp))+
  geom_boxplot(size = 1) + # boxplot with thicker lines
  ggtitle(" Air Temperature") + # title
  xlab(NULL) + # no x label
  scale_y_continuous(name = "Degrees F",   # to customize y axis scale
                     breaks = seq(55, 100, 10), # sequence: lower and upper bounds with interval
                     limits=c(55, 100)) + # limits
  theme_bw() + # add black and white theme 
  theme(plot.title = element_text(size = 14, face = "bold"), # increase title size and make bold
        axis.title = element_text(face="bold")) # axis label bold

# visualize ozone
ggplot(airquality, aes(x="", y=Ozone))+
  geom_boxplot(size = 1, notch = TRUE, fill = "orange") + # boxplot, thicker lines, add notch around median, orange
  ggtitle("Ozone Concentration") + # title
  xlab(NULL) + # no x label
  scale_y_continuous(name = "ppb",   # to customize y axis scale
                     breaks = seq(0, 170, 25),   # seqience: lower bound, upper bound, and intervals
                     limits=c(0, 170)) + # limits
  theme_bw() + # add black and white theme
  theme(plot.title = element_text(size = 14, face = "bold"), # increase size of title and make bold
        axis.title = element_text(face="bold")) # make axis title bold

# visualize ozone across and x -axis the length of the ozone vector
ggplot(airquality, aes(y = Ozone, x = seq(1, length(airquality$Ozone)))) + 
  geom_point() + # scatter
  ggtitle("Mean Ozone Concentration") + # titles
  xlab("Index") +
  ylab("Ozone in ppb")

# visualize ozone across and x -axis the length of the ozone vector
ggplot(airquality, aes(y = Ozone, x = seq(1, length(airquality$Ozone)))) + 
  geom_point() +  #scatter
  ggtitle("Mean Ozone Concentration") + # titles
  xlab("Index") +
  ylab("Ozone in ppb") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
        axis.line.y = element_line(size = 0.5, colour = "black"),
        panel.grid.major.y = element_line(color = "gray"), # grey grid lines
        axis.text = element_text(face = "italic"),  # italic values on scales
        panel.grid.major = element_blank(), # erase horizontal grid lines
        panel.grid.minor = element_blank(), # erase verticle grid lines
        panel.border = element_blank(),     # erase border
        panel.background = element_blank()) # make background white

# visualize ozone across and x -axis the length of the ozone vector
ggplot(airquality, aes(y = Ozone, x = seq(1, length(airquality$Ozone)))) + 
  geom_point(aes(colour = factor(Month))) +  # scatter where points are coloured by month
  ggtitle("Mean Ozone Concentration") + # titles
  xlab("Index") +
  ylab("Ozone in ppb") +
  theme(legend.position="none") + # no legend
  annotate("rect", xmin = 35, xmax = 55, ymin = 5, ymax =80,  # add an overlayed rectangle, specifying location on x and y
           alpha = 0.2) # make transparent