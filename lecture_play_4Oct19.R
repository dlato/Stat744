## graphics
library(ggplot2)
#below will remove the margins in facet, basically makes it look nicer
#can change this to be what you want
# can use panel.spacing.x or .y to change it in just one direction
theme_set(theme_bw()+theme(panel.spacing=grid::unit(0,"lines")))
library(directlabels)
## modeling/coef plots
library(lme4)
#next 2 are tidy-ish, they can be used to make coefficent plots
library(broom)
library(broom.mixed)
#package for creating coefficent plots (below)
#*should* be scaling everything by 2 SDs so that they are all the same units
# so it will put everything into the response var units (mpg in cars) per 2 SDs, so really not real units
library(dotwhisker)
library(ggstance) ## horizontal geoms
library(stargazer)
## manipulation
library(tidyverse)

#in ggplot below will reverse the legend data order (prettier)
#guide=guide_legend(reverse = TRUE)
