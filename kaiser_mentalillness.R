## Data from: https://www.healthsystemtracker.org/chart-collection/current-costs-outcomes-related-mental-health-substance-abuse-disorders/#item-eighteen-percent-adults-united-states-mental-behavioral-emotional-disorder

library(tidyverse)
library(ggalt)
dd <- (read_csv("data/data-fHPJK.csv")
       #so here we are adding in some stuff about the datafile so that we can use it properly
       # so the way it is set up right now is bad so we are trying to fix it
    %>% mutate(category=rep(c("overall","sex","age","ethnicity"),
                            times=c(1,2,3,6)),
               category=factor(category,levels=unique(category)))
    #re-naming the variable names so that there are no spaces so that its good for coding
    #still fixing up the dataframe so it looks nice
    %>% rename(pct="Percent of adults with AMI")
    %>% group_by(category)
    #below is a bit advanced, but basically we want to make sure that within each category
    #we want the order to be decreasing
    #so we are arrancing by decreasing pct in each category
    #order of categories was determined aboce on line 8 (in mutate())
    %>% arrange(category,desc(pct))
    %>% ungroup()
    #then making group a factor which is taking the above 2 lines and embedding it in the order of the group factors
    #being a bit clever here and having foresight into what we want this to look like
    %>% mutate(Group=factor(Group,levels=rev(Group)))
)
library(ggplot2); theme_set(theme_classic())

#trying some things first
(ggplot(dd, aes(fill = category, x=Group, y=pct))
    + geom_bar(stat = "identity")
)




ggplot(dd,aes(colour=category,x=Group,y=pct))+
    scale_x_discrete()+
    geom_lollipop(size=5) + coord_flip() +
    theme(legend.position="none") +
    labs(x="",y="Percent of adults with AMI")+
    scale_colour_brewer(palette="Dark2")
## ggsave("kaiser_mentalillness.png")
