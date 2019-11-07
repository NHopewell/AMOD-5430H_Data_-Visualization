## Data Visualization Assignment 4
## Nicholas Hopewell - 0496633

library(ggplot2)
library(ggmap)
library(datasets)
library(dplyr)
library(tidyr)

#### Question 4: ####

# read in the data
aus_data <- austres
#View(aus_data)

# is it a time series? Two ways to see:
str(aus_data)
is.ts(aus_data)

# show time series data
print(aus_data)

aus_diff_data <- diff(aus_data)
# View(aus_data)

# basic time series plot - Time on the x-axis
# observations are shown from the first on the left, to the last on the right
plot(aus_data, xlab = "Year", ylab = "# (in thousands) of Australian residents", 
     main = "Quarterly measurements of the number of \nAustrialin residents from March 1971 - March 1994")

# show time series data
print(aus_data)
# how many total observations
length(aus_data)

# see start and end
cat(start(aus_data), end(aus_data))

# frequency of measurements
frequency(aus_data)

# see time increment
deltat(aus_data)

# view measurement cycle
cycle(aus_data)

# diff values
aus_diff_data <- diff(aus_data)
# plot difference series
plot(aus_diff_data, xlab = "Year", ylab = "Difference vals", 
     main = "Difference series of the Austres data")

# original length - diff length
cat(length(aus_data), length(aus_diff_data))

# recall the frequency was 4 (4 measurements per year)
decompose_aus = decompose(ts(aus_data, frequency = 4), "additive")

# plot each decomposition and the original data 
#plot(as.ts(decompose_aus$seasonal))
#plot(as.ts(decompose_aus$trend))
#plot(as.ts(decompose_aus$random))
plot(decompose_aus)

# same as before just with a different fitting criteria
stl_aus = stl(ts(aus_data, frequency = 4), "periodic")
seasonal_stl_aus   <- stl_aus$time.series[,1] # taking the first element
trend_stl_aus     <- stl_aus$time.series[,2] # second element
random_stl_aus  <- stl_aus$time.series[,3] # third element

# plotting all the graphs
#plot(aus_data)
#plot(trend_stl_aus)
#plot(as.ts(seasonal_stl_aus))
#plot(random_stl_aus)
plot(stl_aus)

# 2 vectors, one with first element missing, one with last element missing
series_one <- aus_data[-1]
series_two <- aus_data[-length(aus_data)]

# print head of combined data
head(cbind(series_one, series_two))

# plot series
plot(series_one, series_two)

# correlate both series'
cor(series_one, series_two)

# only lag-1
acf(aus_data, lag.max = 1, plot = FALSE)

# get autocorrelations up to lag 12
acf(aus_data, lag.max = 12, plot = FALSE)

# plot ACF
acf(aus_data, lag.max =  12, plot = TRUE)

acf(aus_diff_data, lag.max =  12, plot = TRUE)



#### Question 5  ####

# source of the data:  https://open.canada.ca/data/en/dataset/2c3672b6-4c17-4ff5-9861-29e2dd6d03b3

# read in the text file
EQ_data <- read.table("eqarchive.txt", header = T, sep =",")
#View(EQ_data) # view data

# split date into two new columns, Date and Time
EQ_data <- EQ_data %>%
                separate(date, c("Date", "Time"), "T")

# look at data
glimpse(EQ_data)


# remove last 5 characters from Time column ("+0000")
EQ_data$Time = substr(EQ_data$Time,1,nchar(EQ_data$Time)-5)
EQ_data$Date <- as.Date(EQ_data$Date) # make 'date' a date data type
# EQ_data$Time <- as.POSIXct(EQ_data$Time,format = "%H:%M:%S") 

# check structure of data frame after changes
str(EQ_data)


# filter only most recent dates (2016)
EQ_data <- EQ_data %>%
        filter(Date > "2015-12-31")



# set lat and long to be around the center of Canada and zoom out to see entire country
Canada <- get_map(location = c(-100, 60), zoom =3)



# overlaid scatter of magnitudes
mag_map <- ggmap(Canada) +
                geom_point(size = 3, position = "dodge", data = EQ_data, mapping = aes(x = longitude, y = latitude, color = magnitude, alpha = 0.3)) + # points with dodge for overlap, plot lat, long, magnitude with alpha for transparency 
                scale_colour_gradient(low = "yellow",high = "dark red") + #set colour scale over magnitude scale title scale
                theme(axis.title=element_blank(), # no axis label
                axis.text=element_blank(), # no axis text
                axis.ticks=element_blank()) + # no tick marks
                ggtitle("Magnitudes of Earth Quakes \nAcross Canada (2016)") # main title 

#mag_map <- ggmap(Canada) +
#        geom_point(size = 3, position = "dodge", data = EQ_data, mapping = aes(x = longitude, y = latitude, color = magnitude)) +
#        scale_colour_gradient(low = "yellow",high = "red") +
#        theme(axis.title=element_blank(),
#              plot.title = element_text(size = 22, face = "bold"),
#              axis.text=element_blank(),
#              axis.ticks=element_blank()) +
#        ggtitle("Magnitudes of Earth Quakes Across Canada (2016)\n") 

#mag_map


# density map of occurences
dense_map <- ggmap(Canada) +
                stat_density_2d(bins=400, geom='polygon', size=1.5, data=EQ_data, aes(x = longitude, y = latitude, alpha=..level.., fill = ..level..)) + # density plot of long, lat
                scale_fill_gradient(low = "yellow", high = "red", guide=FALSE) +  scale_alpha(range = c(0.2, 0.9), guide = FALSE) +xlab("") + ylab("") + # set colour scale over magnitude scale, no legend
                theme(axis.title=element_blank(), # no axis label
                axis.text=element_blank(), # no axis text 
                axis.ticks=element_blank()) + # no ticks
                ggtitle("Density of Earth Quake \nOccurences Across Canada (2016)") # main title


# tile map of magnitudes 
tile_map <-  ggmap(Canada) +
                stat_summary_2d(geom = "tile",bins = 65, data=EQ_data, aes(x = longitude, y = latitude, z = magnitude), alpha=0.5) + # tile plot lat, long and magnitude 
                scale_fill_gradient(low = "yellow", high = "red", guide = guide_legend(title = "Magnitude")) +xlab("") + ylab("") + # set colour scale over magnitude scale title scale
                theme(axis.title=element_blank(), # no axis title
                      axis.text=element_blank(), # no axis text
                      axis.ticks=element_blank()) + #  no tick marks
                ggtitle("Earth Quake Clusters \n by Magnitude Across Canada (2016)") # add title

        
        
