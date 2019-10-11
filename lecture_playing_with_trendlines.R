library(tidyverse)
library(lme4) #used to fit complicated model
library(sjPlot)
library(emmeans)
library(effects)

## interplot package?
## https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html

## cf. what's done with splines?

data(Contraception,package="mlmRev")
Contraception <- Contraception %>%
  #making factor for any living children
  mutate(ch=factor(livch != 0, labels = c("N", "Y")))
#fitting a model
m3 <- glmer(use ~ age * ch + I(age^2) + urban + (1 | urban:district),
            data=Contraception, family=binomial)

## effects package
#lots of options, you can tell it which params  you are interested in, how toaverage things...etc
#if you dont know, you can just use the defalts like below
=======
  zeroInt <- function(m,...) {
    v <- vcov(m)
    v["(Intercept)",] <-  v[,"(Intercept)"] <- 0
    return(v)
  }
## doesn't really work
plot(allEffects(m3,vcov. = zeroInt))
#we get lots of ticks in this plot bc we have ppls whos ages are all over the place
#age is centered avg

#if you have interactions btwn 2 or more continuous predictors, things get really complicated
#you would be dealing with a surface and then you have to graph this, which can get messy.
# 3 cont vars would mean you are dealing with a volume...etc
#bc in other cases we can do facets, or colours...etc with categorical vars

#graphs in this package are in the lattice package so its not as cool as ggplot

#
plot(allEffects(m3),partial.residual=TRUE)
plot(Effect("age",m3),partial.residuals=TRUE)
#effects package can also plot partial residuals
#for a univariate model, we could plot orig data and fitted line, and this is more human interpretable to seee the patterns
#but harder to see the little changes, bc the scale is over the full data
#when you try to collapse all your data onto one axis, it does not do the model justice bc it is collapsing the info it is accounting for
#so what below does is the residuals + predicted effect of age (for ex), 
#here it is not super useful, but it jitters, and the effects plot can give  you predicted vals but also what the model thinks the
#data should look like except the predictor that you are looking at atm

plot(predictorEffects(m3,partial.residuals=TRUE),
     partial.residuals=list(pch=".",col="black"))
#below: asking for all effects and then asking for particular values
#produces a list of dataframes: one element for each term in the model
#applied as.data.frame to effects and got out a list of dataframes
#CIs are harder to see on the bottom left graph
dd <- as.data.frame(allEffects(m3,xlevels=list(age=seq(-10,20,length=51))))
View(dd)


#plot it: WIHOUT a scale, when you look at the one on the left it does not look quadratic at all
#take just the dataframe for age by num of children
View(dd[["age:ch"]])
#below with no scale is good for a lay audience bc log scales are confusing
#BUT this is making it hard to see the values that are near zero and 1 bc they are all squished together
ggplot(dd[["age:ch"]],aes(age,fit))+
#drew a line
    geom_line()+
  #split it up by live children
  facet_wrap(~ch)+
  #adding bands for CI, transparent so it looks better
  geom_ribbon(aes(ymin=lower,ymax=upper),colour=NA,alpha=0.2)

#now WITH a scale so its clearer, putting it back on a log odds scale so things look nicer
#plot it
#take just the dataframe for age by num of children
ggplot(dd[["age:ch"]],aes(age,fit))+
  #drew a line
  geom_line()+
  #split it up by live children
  facet_wrap(~ch)+
  #adding bands for CI, transparent so it looks better
  geom_ribbon(aes(ymin=lower,ymax=upper),colour=NA,alpha=0.2)+
  #making our own scale bc the defaults are just log10, sqrt, and reverse. so if you need anything else, you have to use the scales package
  #scales::logit_trans means use the scales package and then the logit_trans func
  #so now they are back to looking quadratic
  #when you use the below log scale, you are using a probability too, but log odds may be hard to understand bc ppl are maybe not able to see that
  #in this scale we see a wider range of probabilities without losing resolution
  #we mostly care about the difference btwn 1 and 2% but not necessaraly 50 and 51%
  #so again, if we really care about probs near zero and 1 then we want to use this scale
  scale_y_continuous(trans=scales::logit_trans(),
                     breaks=c(0.01,0.05,0.1,0.2,0.4,0.6))+
  labs(x="centred age",y="probability of contraception use")


## sjPlot
#this is trying to do everything, coeff plots...etc lots of different kinds of plots
#something about the present model/data does not work
#so below DOES NOT WORK, ben needs to figure it out
pp <- plot_model(m3,type="pred",terms="age[all]")

#so when we print it, it still makes a graph, but there are no CIs on this graph,
#coding wise: its bad that it spits an error but also still produces a graph
print(pp)
## this doesn't work (yet) ???



##########################################################################################################
#now going over the healthcare data that we used in lecture
#pic that originally had so many lines connecting eachother
#Gelman suggested that they should put them as points instead, bc its nicer
#link to the data is from a webpage that does not exist any more
#internet archive (wayback machine) is trying to archive all the stuff on the internet
#really good for URLs that do not exitst any more
#so ben used this so that he could get the data that they claimed to have the data
#so its REALLY important to be honest about your stuff and making  your data avail
#
#so ben got the data from a blog from someone who took it into R and put it in a text format
##########################################################################################################
library(cowplot)
library(ggplot2); theme_set(theme_cowplot())
library(tidyverse)
library(MASS)
library(ggrepel)

#2006 data, except Turkey, where healthexpenditure is 2005
#the data was created using dput() and so this is just extracting it
#it is great to ref your sources and have data automatically download, but be aware that the data might dissapear,
#so  you should always keep a copy of the data yourself so that you can locally re-create what you need!!
#so below we lost the meta-data, dont know units, dont know countries that do not have universal healthcare, so  you should always
#try to keep this
hea = structure(list(healthexpenditure = c(3167L, 3608L, 3356L, 3696L, 1535L, 3357L, 2709L, 3423L, 3464L, 2547L, 1457L, 3207L, 3001L, 2673L, 2581L, 1491L, 4162L, 777L, 3611L, 2398L, 4507L, 920L, 2150L, 1322L, 2466L, 3124L, 4165L, 618L, 2885L, 6933L), lifeexpectancy= c(81.1, 79.9, 79.5, 80.7, 76.7, 78.4, 79.5, 80.7, 79.8, 79.6, 73.2, 81.2, 79.8, 81.4, 82.4, 79.1, 79.4, 74.8, 79.8, 80.1, 80.5, 75.3, 78.9, 74.3, 81.1, 80.8, 81.7, 73.2, 79.5, 78.1)), .Names = c("healthexpenditure", "lifeexpectancy"), class = "data.frame", row.names = c("Australia", "Austria", "Belgium", "Canada", "Czech Republic", "Denmark", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Japan", "Korea", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"))

#in tidyverse, it does not like rownames, but below is a nice func that will take the rownames and put them into a col that you want
#better for graphing
hea2 <- (hea
         %>% rownames_to_column("country")
)
hea2

#first attempt that gets the basics, that is really ugly 
print(ggplot(hea2, aes(healthexpenditure,lifeexpectancy,label=country))
      + geom_text()
)

#fixing scale and labs so that they have units 
print(ggplot(hea2, aes(healthexpenditure,lifeexpectancy,label=country))
      + geom_text()
#anchoring scale by zero
      + scale_x_continuous(limits = c(0,NA))
      + labs(y= "life expectancy (year)",
             x= "health expenditure (PPP$)")
)

#fixing overlapping labels, quick and dirty way that would require some playing
print(ggplot(hea2, aes(healthexpenditure,lifeexpectancy,label=country))
      + geom_text()
      + scale_x_continuous(limits = c(0,NA))
      #can also add bubble around the text so its easy to see where they are point to, could add a fill
      + geom_text_repel(size=3, vjust=-1.5)
      + labs(y= "life expectancy (year)",
             x= "health expenditure (PPP$)")
)

#now playing with a trendline
#but this really does not look great bc it implies that the more money you spend it eventually decreases life exp for everyone
#which seems wrong
print(ggplot(hea2, aes(healthexpenditure,lifeexpectancy,label=country))
      + geom_text()
      + geom_smooth()
)


#adding a more complicated smooth line that is linear reg
#worried that the US is an outlier that is pulling down the regression line
print(ggplot(hea2, aes(healthexpenditure,lifeexpectancy,label=country))
      + geom_text()
      #making a robust regression: extremem values still contribute to residuals, but they are not weighted as much
      #still seems to be some biased in this regression missig the low points
      + geom_smooth(method="rlm")
      #so also including this line below, will make an extra line (red) while including the US
      + geom_smooth(method="lm",lty=2,colour="red")
)
