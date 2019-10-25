load("taxdata.RData")
library(dplyr)
library(ggplot2)
library(plotly)

xlabs <- c("0-10th", "20th-30th", "40th-50th", "60th-70th",
           "80th-90th", "95th-99th", "99.99th", "Top 400")
xbr <- seq(0, 14, 2)

theme_set(theme_bw())
gg0 <- (tax
        #below is a plotly command that augments this datafram so that we can hilight specific years
        %>% highlight_key(~year)
        #income on x and taxrate on y
        #colouring and grouping by year and income group
        %>% ggplot(aes(income_group,total_tax_rate,
                       colour=year,group=year))
        + geom_line(size=2)
        + labs(x="Income group", y="Total tax rate")
        + scale_x_continuous(labels=xlabs, breaks=xbr)
)
#again plotting everything ontop of eachother but NO interactive-ness
gg0

(gg0
  %>% ggplotly(tooltip="group")
  %>% highlight(on="plotly_hover",
                off=NULL,
                color="gold",
                #I think this means that you can pick your brush colour for the trendline you are hilighting
                dynamic=TRUE)
)

#hover fades the things that you are NOT hovering over, automatically.
#which is good!


#if you wanted to share this with other ppl, you can save it with html widget? or something that would do this
#can also knit it into html


#3D
#below is rendered in separate window, but there is a wigit for this and you can use to do this (htmlwidgits)
#to turn this into a javascript that you can then put into a doc
library(rgl)
persp3d(volcano)
demo("rgl")

