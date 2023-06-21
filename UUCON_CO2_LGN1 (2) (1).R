#library
library(tidyverse)
library(zoo)

#Path on CHPC
uucon_path = "../UUCON_CO2/"

#Rolling calculation length? Assumes our data is hourly (which it is for this case)
rwidth = 24*3

#Read in data
Data2015<-read.csv("UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2015 (1).csv")
Data2016<-read.csv("UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2016.csv")
Data2017<-read.csv("UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2017.csv")
Data2018<-read.csv("UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2018.csv")
Data2019<-read.csv("UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2019 (2).csv")
Data2020<-read.csv("UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2020 (1).csv")
Data2021<-read.csv("UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2021 (1).csv")

#switch column "CO2_LG2" t "CO2_LNG" for Data2021
colnames(Data2021)[colnames(Data2021)=="CO2_LG2"]<-"CO2_LGN"

#selecting the "Time_UTC" and "CO2_LGN" columns
Data2015LGN<-Data2015 %>% select(5, 11)
Data2016LGN<-Data2016 %>% select(5, 13)
Data2017LGN<-Data2017 %>% select(5, 12)
Data2018LGN<-Data2018 %>% select(5, 12)
Data2019LGN<-Data2019 %>% select(5, 13)
Data2020LGN<-Data2020 %>% select(5, 13)
Data2021LGN<-Data2021 %>% select(5, 12)

#Combine data with rbind
Data<-rbind(Data2015LGN, Data2016LGN, Data2017LGN, Data2018LGN, Data2019LGN, Data2020LGN, Data2021LGN)

#Saving Time_UTC column as.POSIXct
Data$Time_UTC<-as.POSIXct(Data$Time_UTC, format="%Y-%m-%d%H:%M:%S",tz="GMT")

#Converting data to MST
Time_MST <- Data$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5]

#Computing rolling "5th percent quantile". Set width to 3 days (3*24) for 3 day rolling average,
#have it remove NaNs from the calculating, and lastly, set fill to NA so R automatically fills in
#sides of data range so we perserve our vector length so it matches our time vector. 
Baseline_Co2 <- rollapply(Data$CO2_LGN,width=rwidth,FUN="quantile",p=.05,na.rm=T,fill=NA)
plot(Baseline_Co2)

#adding the baseline
Data$Time<-as.numeric(Data$Time_UTC)
Time<-c()
Baseline_Co2<-c()
for(i in Data$Time){
  subdata<-Data[abs(i-Data$Time) <129600,]
  quantile<-quantile(subdata$CO2_LGN, 0.05, na.rm=TRUE)
Baseline_Co2<-c(Baseline_Co2, quantile)
}


#Creating and customizing the graph
plot(Data$Time_UTC, Data$CO2_LGN, xlab="Year", ylab="CO2 [ppm]", main="5th Percentile Baseline of CO2 Emissions in Logan, Utah between 2015-2021", type="l")

#plotting baseline
lines(Data$Time_UTC, Baseline_Co2, col="red")
