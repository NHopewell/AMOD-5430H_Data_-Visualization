
# load packages
require(plyr)
require(reshape2)
library(ggplot2)
library(corrplot)
library(corrgram)

# set theme to black across the board
theme_set(theme_bw())


####### Question 1

# read in the data
edLevelData <- read.csv("refugeeEdLevel.csv", header = TRUE) 

# view data
View(edLevelData)

library(dplyr)
 edLevelData %>% group_by(Education.Level) %>% summarise(tot_Amount = sum(Amount))

## education level: 
 
 # bar plot
ggplot(edLevelData, aes(x = factor(reorder(Education.Level, Amount)), y =  Amount, fill=factor(Refugee.status))) +  # fill colour variable
        geom_bar(stat="identity") +
        labs( title = 'Education Level of Syrian Refugees', # labels
              x = "\nEducation Level\n",
              y = "\nCount\n",
              fill = "Refugee Status" ) + # legend label
        theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # set plot elements as blank
                panel.background = element_blank(), axis.line = element_blank(), 
                axis.text = element_text(face="bold", size=12), axis.title = element_text(face="bold", size=14), # custom style, bold and font size
                legend.text = element_text(face="bold", size = 10),
                legend.title = element_text(face="bold", size = 12)) +
        coord_flip() # flip the plot coordinates to make horrizontal




## languages

# create the data set
langs <- cbind.data.frame(c('English', 'French', 'Both', 'Neither'), c(27425, 2355, 660, 58140))

# give column names
colnames(langs) <- c("Language", "Amount")

# view it
View(langs)

# check to see it worked
str(langs) 

# bar plot
ggplot(langs, aes(x = reorder(Language, -Amount), y = Amount, fill = Language)) + # fill colour variable
        geom_bar(stat="identity") +
        labs( title = '', # rtitles 
              y = "",
              x = "Language spoken") +
        theme(legend.position="none") # get rid of legend



###### Question 2


# make the new threshold variables with ifelse: if experiesion true = 1, else =0 
lifeCycleSavings <- mutate(LifeCycleSavings,
                           young_pop = ifelse(pop15 >= 40, 1,0),
                           old_pop = ifelse(pop75 >= 3, 1, 0), 
                           high_save = ifelse(sr >= 10, 1, 0), 
                           fast_growth = ifelse(ddpi >= 3, 1, 0),
                           high_income = ifelse(dpi >= 1000, 1, 0))

# take row names from original data 
rowNames <- rownames(LifeCycleSavings)
# give these row names to modified data
rownames(lifeCycleSavings) <- rowNames


# make the first scatter plot
p1 <- gplot(lifeCycleSavings, aes(ddpi, sr)) +
        geom_point()

# second scatter plot
P2 <- ggplot(lifeCycleSavings, aes(ddpi, sr)) +
        geom_point(aes(color = factor(young_pop))) + # colour grouping variable
        scale_colour_brewer(palette = "Set1")

# now it works for colour and black and white print
# P3 <- ggplot(lifeCycleSavings, aes(ddpi, sr)) +
#         geom_point(aes(shape = factor(young_pop), color = factor(young_pop))) +
#         scale_colour_brewer(palette = "Set1")
# 
# 
# 
# P4 <- ggplot(lifeCycleSavings, aes(ddpi, sr)) +
#         geom_point(size = 3, aes(shape = factor(young_pop), color = factor(young_pop))) +
#         scale_colour_brewer(palette = "Set1") +
#         theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
#                axis.line.y = element_line(size = 0.5, colour = "black"))


# fifth scatter plot 
P5 <- ggplot(lifeCycleSavings, aes(ddpi, sr)) +
        geom_point(size = 3, aes(shape = factor(young_pop, labels = c('< 40%',' >= 40%')), #levels of the grouping variable
                                   color = factor(young_pop, labels = c('< 40%',' >= 40%')))) +
        scale_colour_brewer(palette = "Set1") + # colour palette
        labs( title = 'Life Cycle Savings',  # titles
              x = '% growth rate of per-capita disposable income',
              y = 'Personal savings rate',
              color = "% of population\n below age 15", # legend titles
              shape = "% of population\n below age 15") +
        theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
               axis.line.y = element_line(size = 0.5, colour = "black"))

#gridExtra::grid.arrange(p1, p2, p3, p4, p5, nrow = 1)



# notice cluster towards bottom:
which(lifeCycleSavings['sr'] <= 6 & lifeCycleSavings['young_pop'] == 1)

# subset the data to see which countries these are:
lifeCycleSavings[which(lifeCycleSavings['sr'] <= 6 & lifeCycleSavings['young_pop'] == 1), ]


# same scatter pkot but with annotation
ggplot(lifeCycleSavings, aes(ddpi, sr)) +
        geom_point(size = 3,aes(shape = factor(young_pop, labels = c('< 40%',' >= 40%')),  #levels of the grouping variable
                                 color = factor(young_pop, labels = c('< 40%',' >= 40%')))) +
        scale_colour_brewer(palette = "Set1") + # colour palette
        labs( title = 'Life Cycle Savings',  # titles
              x = '% growth rate of per-capita disposable income',
              y = 'Personal savings rate',
              color = "% of population\n below age 15",  # legend titles
              shape = "% of population\n below age 15") +
        theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
               axis.line.y = element_line(size = 0.5, colour = "black")) +
        annotate("rect", xmin = -.2, xmax = 6.3, ymin = 1.6, ymax =6.3,  # add an overlayed rectangle, specifying location on x and y
                 alpha = 0.3) +
        annotate("text", x = 8.5, y = 4, label = " Low sr \n young pop \n cluster")# add text




#  looking at only south american
# subset only south american countries
southAmerica <- lifeCycleSavings[c('Brazil', 'Colombia', 'Costa Rica', 'Ecuador', 
                                   'Guatamala', 'Honduras', 'Nicaragua', 'Panama', 'Paragua',
                                   'Peru', 'Venezuela', 'Chile', 'Uruguay'), ]


# similar plot but with only subsetted data
ggplot(southAmerica, aes(ddpi, sr)) +
        geom_point(size = 3,aes(shape = factor(young_pop, labels = c('< 40%',' >= 40%')), #levels of the grouping variable
                                color = factor(young_pop, labels = c('< 40%',' >= 40%')))) +
        scale_colour_brewer(palette = "Set1") + # colour palette 
        labs( title = 'Life Cycle Savings (South America)',
              x = '% growth rate of per-capita disposable income', # titles
              y = 'Personal savings rate',
              color = "% of population\n below age 15", # legend titles
              shape = "% of population\n below age 15") +
        theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
               axis.line.y = element_line(size = 0.5, colour = "black")) +
        geom_text(aes(label=row.names(southAmerica)), size=4, vjust=-2)  # add text for each data point as their row name (country)
        #ggrepel::geom_label_repel(aes(label=row.names(southAmerica)))


# looking at disposible income as well now

ggplot(lifeCycleSavings, aes(ddpi, sr)) +
        geom_point(size = 3,aes(shape = factor(young_pop, labels = c('< 40%',' >= 40%')), #levels of the grouping variable
                                color = factor(high_income, labels = c('< 1000',' >= 1000')))) +
        scale_colour_brewer(palette = "Dark2") + # colour palette 
        labs( title = 'Life Cycle Savings',
              x = '% growth rate of per-capita disposable income', # titles
              y = 'Personal savings rate',
              color = "Real per-capita\n disposable income", # legen titles
              shape = "% of population\n below age 15") +
        theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
               axis.line.y = element_line(size = 0.5, colour = "black"))


# interpret this many attributes

#notice none of the countries with more than 40% of its population under the age of 15 are high income

which(lifeCycleSavings['young_pop'] ==1 & lifeCycleSavings['high_income'] == 1)   




# fitting two linear models based on disposible income of country

 ggplot(lifeCycleSavings, aes(ddpi, sr, color=factor(high_income, labels = c('< 1000',' >= 1000')))) + #levels of the grouping variable
         geom_point() +
         geom_smooth(method= lm, se=FALSE, fullrange = FALSE) + # add linear model with standard error
         scale_color_brewer(palette="Dark2") + # colour palette 
         labs( title = 'Life Cycle Savings',
               x = '% growth rate of per-capita disposable income', # titles
               y = 'Personal savings rate',
               color = "Real per-capita\n disposable income") +  # legend titles
         theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
                axis.line.y = element_line(size = 0.5, colour = "black"))

 
 
 
# As a rug plot
 
 ggplot(lifeCycleSavings, aes(ddpi, sr)) +
         geom_point(size = 3) + # scatter 
         geom_rug() + # add the rug to the plot
         labs( title = 'Life Cycle Savings', # titles
               x = '% growth rate of per-capita disposable income',
               y = 'Personal savings rate') +
         theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
                axis.line.y = element_line(size = 0.5, colour = "black"))
 
 
 # as a bubble plot, size = dpi
 
 ggplot(lifeCycleSavings, aes(pop75, sr, size = dpi)) + # buble size = dpi
         geom_point(shape=21, color='black', fill='blue', alpha = 0.6) + # blue, transparent bubbles
         #scale_colour_brewer(palette = "Set1") +
         scale_size_area(max_size=15) + # set max bubble size
         labs( title = 'Life Cycle Savings', # titles
               x = '% of population above age 75',
               y = 'Personal savings rate')+
         theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
                axis.line.y = element_line(size = 0.5, colour = "black"))

 
 # scatter plot matrix:
 
 pairs(lifeCycleSavings[,1:5])
 
 
 # correlation matrix
 
 corMatrix <- cor(lifeCycleSavings)
 corrplot(corMatrix, method = "square", shade.col = NA, tl.col = "black", tl.srt = 45)
 
 # second, smaller matrix
 corMatrix2 <- cor(lifeCycleSavings[, 1:5])
 corrgram(corMatrix2, order = TRUE, lower.panel = panel.shade, upper.panel = NULL, # no uppwe panel 
          text.panel = panel.txt, main = "Life Cycle Savings") # title

 
 # small multiples: MUST COMBINE THESE
 
 # split by high income:
 
 ggplot(lifeCycleSavings, aes(factor(high_income, labels = c('< 1000',' >= 1000')), pop75, fill=factor(high_income))) + # grouping variable
         geom_boxplot() + # boxplot
         guides(fill=FALSE) + # no legend
         labs( title = '% of population above age 75', # titles
               x = "Real per-capita disposable income",
               y = '')+
         theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
                axis.line.y = element_line(size = 0.5, colour = "black"))
 
 
 
 ggplot(lifeCycleSavings, aes(factor(high_income, labels = c('< 1000',' >= 1000')), pop15, fill=factor(high_income))) + # groyping varible
         geom_boxplot() + # boxplpt
         guides(fill=FALSE) + # no legend
         labs( title = '% of population below age 15', # labels
               x = "Real per-capita disposable income",
               y = '')+
         theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
                axis.line.y = element_line(size = 0.5, colour = "black"))
 
 
 ggplot(lifeCycleSavings, aes(factor(high_income, labels = c('< 1000',' >= 1000')), sr, fill=factor(high_income))) + # grouping variable
         geom_boxplot() + # boxplot 
         guides(fill=FALSE) + # no legend
         labs( title = 'Personal savings rate', # labels
               x = "Real per-capita disposable income",
               y = '')+
         theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
                axis.line.y = element_line(size = 0.5, colour = "black"))
 # arrange with gradextra 
 
 # split by young_pop
 
 ggplot(lifeCycleSavings, aes(factor(young_pop, labels = c('< 40%',' >= 40%')), dpi, fill=factor(young_pop))) +  # grouping variable
         geom_boxplot() +
         guides(fill=FALSE) + # no legend
         labs( title = 'Per Capita Disposible Income', # labels
               x = '% of population below age 15',
               y = "") +
         theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
                axis.line.y = element_line(size = 0.5, colour = "black"))
 
 ggplot(lifeCycleSavings, aes(factor(young_pop), ddpi, fill=factor(young_pop, labels = c('< 40%',' >= 40%')))) +  # grouping variable
         geom_boxplot() + # boxplot
         guides(fill=FALSE) + # no legend
         labs( title = '% growth rate of per-capita disposable income', # labels
               x = '% of population below age 15',
               y = "") +
         theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
                axis.line.y = element_line(size = 0.5, colour = "black"))
 
 ggplot(lifeCycleSavings, aes(factor(young_pop), sr, fill=factor(young_pop, labels = c('< 40%',' >= 40%')))) +  # grouping variable
         geom_boxplot() + # boxplot
         guides(fill=FALSE) + # no legend
         labs( title = 'Personal savings rate', # labels
               x = '% of population below age 15',
               y = "") +
         theme( axis.line.x = element_line(size = 0.5, colour = "black"), #give axes lines, specify size and colour
                axis.line.y = element_line(size = 0.5, colour = "black"))
 
 # arrange with gradextra
 
 
 
 ## ICON plots
 
 # load packagges
 library(symbols)
 library(aplpack)
 
 # star plot
 
 # load packages
 require(ggplot2)
 require(ggiraph)
 
 
 stars(lifeCycleSavings[1:10, 1:5]) # first 10 countries, only original variables 
 
 stars(lifeCycleSavings[4:6, 1:5],locations = c(0, 0), radius = FALSE, # only 3 countries
       col.lines = 1:10, key.loc = c(0, 0), main = "Life Cycle Savings", lty = 2) # line types ad colours
 
 
 
 # chernoff faces
 
 faces(lifeCycleSavings[, 1:5] , labels = rownames(lifeCycleSavings)) # only original variables
 
 faces(lifeCycleSavings, labels = rownames(lifeCycleSavings)) # all variables 
 
 
 #thermometers(lifeCycleSavings[1:10, 1:5])
 
 
 # pie chart matrix
 
 corMatrix <- cor(lifeCycleSavings)
 corrplot(corMatrix, method = "pie", shade.col = NA, tl.col = "black", tl.srt = 45) 
 
 

         