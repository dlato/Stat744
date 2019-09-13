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
