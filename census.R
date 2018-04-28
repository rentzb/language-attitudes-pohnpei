setwd("~/Documents/UH/dissertation/r_code")
library(tidyverse)
library(hrbrthemes)
library(strengejacke)

data <- read.csv("census_lang.csv")

# plot languages spoken at home

ggplot(data,aes(x=year,y=prop,color=language)) + 
  geom_point() + theme_ipsum() + geom_smooth(method="lm",fullrange=T,se=T) +
  xlim(1990,2060)  + scale_y_percent() + labs(x="Year", y="Percentage of Pohnpei Proper population",
                                             title="Language spoken at home on Pohnpei",
                                             caption="1994, 2000, and 2010 data from FSM Census") +
  scale_color_ipsum()
ggsave("censusLanguage.png",device="png",dpi=500,width=6,height=4,units="in")
