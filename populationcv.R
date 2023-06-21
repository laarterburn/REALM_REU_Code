#libraries that we need to load.
library(dplyr)
library(zoo)
library(tidyverse)
library(lubridate)

#reading in the file
#file.choose()

#Plotting and reading in for Cache Valley 
Cachepop<-read.csv("/uufs/chpc.utah.edu/common/home/u6036967/Downloads/Cache_Valley_Population(3).csv")
plot(Cachepop, main="Cache Valley Population from 2000-2019", ylab="Population", type="l")

#making trendline
lm_cache<-lm(Cachepop$Population ~ Cachepop$Year)
abline(lm_cache, untf=FALSE, col="red")
slopecache <- signif(lm_cache$coef[[2]], 3)
xpcache <- signif(summary(lm_cache)$coef[2,4], 3)
legend(x="bottomright", y=2, legend=c(paste("P-value:",xpcache), paste("Slope:",slopecache)))
