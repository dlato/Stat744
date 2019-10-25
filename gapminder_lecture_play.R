##########################
# playing in class Oct 25
# making an animation!
#warning: you do need to instal a bunch of things to make it actually work on your maching. but eventually you can figure it out
##########################
#remember when you are making animations you also want to think about the rules for the graph that we apply to
#non-animated graphs



## https://github.com/keithmcnulty/hans_rosling_bubble/blob/master/rosling.R
# packages

library(tidyverse)
library(ggplot2)
library(viridis)
library(gganimate)
#getting data from the world bank
library(wbstats)
#gifski is what we need to use to stitch the frames together, so we need this to make the animation
## need gifski *or* magick package, *or* something ...
#image magic is well curated and supported and can does the same thing

# Rosling gapminder chart: previously in one command ...

# pull the country data down from the World Bank - three indicators
#below is grabbing the data from the website using API!!! you do need to know exactly what you are looking for
#so you can get the correct data you want (or download the data-dictionarly that would hopefully say this)
wbdata <- wbstats::wb(indicator = c("SP.DYN.LE00.IN", "NY.GDP.PCAP.CD", "SP.POP.TOTL"), 
                      country = "countries_only", startdate = 1960, enddate = 2018)

#joining the data set we got from the website, with data that already exists
wbdata <- (wbdata
           ## pull down mapping of countries to regions and join
           %>% dplyr::left_join(wbstats::wbcountries()
                                #pulling out the iso (standard country identifier code)
                                #this is the data that we are getting from the package
                                #adding this info to the dataset we get from the web
                                %>% dplyr::select(iso3c, region),
                                by="iso3c")
           # spread the three indicators (make 3 separate cols for each GDP)
           %>% tidyr::pivot_wider(id_cols = c("date", "country", "region"),
                                  names_from = indicator, values_from = value)
)
#saving a copy for when we dont have an internet connection
save("wbdata",file="wbdata.rda")

load("wbdata.rda")
#if you have columns with spaces you need to use backticks so ggplot does not get confused
## plot the data
gg0 <- (ggplot(wbdata,
               #taking a log of the data, we should actually just be changing the scale! not the data!
               aes(x = log(`GDP per capita (current US$)`),
                   y = `Life expectancy at birth, total (years)`,
                   size = `Population, total`,
                   group=country))
        #messing with sizes
        + geom_point(alpha = 0.5, aes(color = region))
        + scale_size(range = c(.1, 16), guide = FALSE)
        + scale_x_continuous(limits = c(2.5, 12.5))
        + scale_y_continuous(limits = c(30, 90))
        + viridis::scale_color_viridis(discrete = TRUE,
                                       name = "Region", option = "viridis")
        + labs(x = "Log GDP per capita",
               y = "Life expectancy at birth")
        + theme_classic()
        #putting the text into the middle of the graph for our animation
        + geom_text(aes(x = 7.5, y = 60, label = date),
                    #trying to make a fancy text font called Oswald
                    size = 14, color = 'lightgrey', family = 'Oswald')
)
#this is raw material for the animation, its ALL the years overlayed at once.
#thats why we see this weird font
gg0

#when you make an annimation it is important to tell ggplot which things should be changing across frames.
#so when we put group = country means interpolate the track of each country separatly

if (require("gifski")) {
  ## animate it over years
  #can also choose how fast you want it to play, how fast you want the frames to switch between eachother...etc
  gg1 <- gg0 + gganimate::transition_states(date,
                                            transition_length = 1, state_length = 1) +
    gganimate::ease_aes('cubic-in-out')
  animate(gg1,renderer=ffmpeg_renderer())
  
  anim_save("gapminder1.gif")
  ## anim_save("gapminder1.mp4",renderer=av_renderer())
  
}
## gifski needs Rust installed!
## rendering takes about 30 seconds
