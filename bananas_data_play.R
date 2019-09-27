#making a really terrible graph better
#based on this:
# https://github.com/thomasdebeus/colourful-facts/blob/develop/projects/Redesign-of-worst-chart-ever/redesign-of-worst-chart-ever.md
#######################
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(directlabels)
#######################

#read in data after downloading
df <- read.csv("FAOSTAT_data_1_7_2018.csv")

#plotting just to see what we have
#recognizing that we really only have 3 categories, Country, Year and Value

g1 <- ggplot(df, aes(x = Year, y = Value, color = Country))+
  geom_line()+
  theme_bw() 

plot <- ggplot(df, aes(x = Year, y = Value, color = Country))+
  geom_line()+
  theme_bw() +
  theme(legend.position = "none") +
  geom_dl(aes(label = Country), method = list("last.points"))

plot + xlim(1995, 2010)

g1 + facet_wrap(~Country)

#now ordering the data so that in facet it looks nicer and makes more sense instead of alphbetical
#reodering levels, not values
df$Country <- with(df, reorder(Country, Value, FUN = mean))
#reordering the opposite direction
df$Country <- with(df, reorder(Country, Value, FUN = -x))

g1 <- ggplot(df, aes(x = Year, y = Value, color = Country))+
  geom_line()+
  theme_classic() +
  theme(legend.position = "none")

g1 + facet_wrap(~Country)




#####################################################
#Sep 27: playing with bananas again
#####################################################
#trying to make this same bananas data into a heat map

#below are colours scales, viridis is a new one
library(colorspace)
library(viridis)

#sometimes ggplot does not define colour as you would think, sometimes its fill instead
#presumably you could make log breaks in the colour when you make a heat map, which in this case woud help with this visualization

#the year has decimals bc the year is a continuous var, so you should add in breaks to make it better
#coord flip is good so that way you can easily flip the axis at the last min
coord_flip()
#so anything that is inhearitantly vertical, you need to use coord_filp() to switch it (like box plots)
#but, this does not play nice with facet, so you have to use ggstance
#where you can then use geom_boxh where the h means horizontal

#if doing anything in tidyverse then you are working in a dataframe, so if you use the fct_reorder()
#that is working on a variable and therefore you need to mutate the tidyverse dataframe to 
#then become a variable 

#remember if you flip the axis then sometimes your x in the ggplot becomes your y

#wilkie says that you should not have white spaces, so you should fill in your box plots with some sort of
#neutral colour so it is distinguished from the others
#aes() means asthetic, which is a way to map data to a graph

#so if the log scale squishes things too much, you can try to use square root scale
# but this is confusing to know how to interpret it

#can define your theme as my_theme so that you dont have to keep re-defining it for each graph you make
#once your graph looks roughly the way you want you should go back and clean up your code and remove redundancy

#when using jitter, it will jitter in the x and y axis, which is not what we want sometimes
#so as always, it is best to specify
#dodge basically is the same but says, only move something if you have to

#expand() can tell you how much space to have around the data
#so talking about white space btwn the xaxis and data and how we usually dont want it
#usually goes into the the scale_x_continuous()

#so you should never have data hard coded in your code, try to have it read in as a separate csv
# if you do have to hard code it in, you should put all that at the top of you file as a variable
# where it is really clear that you are hard coding these things
# the best way to do this is to actually add it to the CSV so that it is part of the datafile
# 

#if you want to do facets with different sizes its really not that hard apparently 

#some aspects of graphs do not scale properly, so you should decide what width you want
#it to be (below)
dev.new(width=5, height=7)
#so you should make a separte graph window of that size and export so that you get the exact size of
#things that you want

#openGL is a general thing that oppens 3D graphics
