#libraries that we need to load.
library(dplyr)
library(zoo)
library(lubridate)

#reading in files with 'file.choose()'

#Rolling calculation length? Assumes our data is hourly (which it is for this case)
rwidth = 24*3

#Read in data, where we read in the header and seperate the columns by the ","
Data2015<-read.csv("/uufs/chpc.utah.edu/common/home/u6036967/CO2/UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2015 (1).csv")
Data2016<-read.csv("/uufs/chpc.utah.edu/common/home/u6036967/CO2/UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2016.csv")
Data2017<-read.csv("/uufs/chpc.utah.edu/common/home/u6036967/CO2/UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2017.csv")
Data2018<-read.csv("/uufs/chpc.utah.edu/common/home/u6036967/CO2/UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2018.csv")
Data2019<-read.csv( "/uufs/chpc.utah.edu/common/home/u6036967/CO2/UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2019 (2).csv")
Data2020<-read.csv("/uufs/chpc.utah.edu/common/home/u6036967/CO2/UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2020 (1).csv")
Data2021<-read.csv("/uufs/chpc.utah.edu/common/home/u6036967/CO2/UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2021 (1).csv")

#change column name for 2021
colnames(Data2021)[colnames(Data2021)=="CO2_LG2"]<-"CO2_LGN"

#selecting the "Time_UTC" and "CO2_LGN" columns
Data2015LGN<-Data2015 %>% select(5, "CO2_LGN")
Data2016LGN<-Data2016 %>% select(5, "CO2_LGN")
Data2017LGN<-Data2017 %>% select(5, "CO2_LGN")
Data2018LGN<-Data2018 %>% select(5, "CO2_LGN")
Data2019LGN<-Data2019 %>% select(5, "CO2_LGN")
Data2020LGN<-Data2020 %>% select(5, "CO2_LGN")
Data2021LGN<-Data2021 %>% select(5, "CO2_LGN")

#Merge data with rbind command
Data<-rbind(Data2015LGN, Data2016LGN, Data2017LGN, Data2018LGN, Data2019LGN, Data2020LGN, Data2021LGN)

#Saving Time_UTC column as.POSIXct
Data$Time_UTC<-as.POSIXct(Data$Time_UTC, format="%Y-%m-%d%H:%M:%S",tz="GMT")

#Converting data to MST. Print times as a sanity check-
Time_MST <- Data$Time_UTC
print(Time_MST[1:5])
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
print(Time_MST[1:5])

#Computing rolling "5th percent quantile". Set width to 3 days (3*24) for 3 day rolling average,
#have it remove NaNs from the calculation, and lastly, set fill to NA so R automatically fills in
#sides of data range so we perserve our vector length so it matches our time vector. 
Baseline_Co2 = rollapply(Data$CO2_LGN,width=rwidth,FUN="quantile",p=.05,na.rm=T,fill=NA)

#Lets plot up baseline to make sure it looks reasonable
plot(Data$Time_UTC, Data$CO2_LGN, xlab="Year", ylab="CO2 [ppm]", main="5th Percentile Baseline of CO2 Emissions in Logan, Utah between 2015-2021", type="l")

#plotting baseline
lines(Data$Time_UTC, Baseline_Co2, col="red")

#Merge data back together
new_data = data.frame(Data,Baseline_Co2,Time_MST)

#Grab the hour since we need this when aggregating our data by time of day
hourMST = hour(new_data$Time_MST)
monthMST = month(new_data$Time_MST)

#Compute the CO2 enhancement by substracting out our "background"
co2_enhan = new_data$CO2_LGN - new_data$Baseline_Co2


#Merge even more stuff!
new_data = data.frame(Data,Baseline_Co2,Time_MST,hourMST,monthMST,co2_enhan)


#Loop through each month and make a diurnal plot of CO2 enhancements 
my_months = unique(monthMST)

for(m in 1:length(my_months)){
  
  sub_data = new_data[new_data$monthMST==my_months[m],]
  
  hourly_dat = aggregate(sub_data$co2_enhan,by=list(sub_data$hourMST),FUN=mean, na.rm=TRUE)
  colnames(hourly_dat) <- c("hour","co2_enhancement")
  
  png(paste0("diurnal_co2_month_",my_months[m],".png"), 600, 600,pointsize=24); par(mar=c(5,5,5,5))
  plot(hourly_dat$hour,hourly_dat$co2_enhancement,type="l",xlab="Time [MST]",ylab="CO2 [ppm]",lwd=2,main=paste0("Month =",my_months[m]))
  dev.off()
  
}

#define our seasons... Based on meteorological seasons 

seasons = matrix(c(12,seq(1,11,by=1)),nrow=3,ncol=4)
colnames(seasons) <- c("Winter","Spring","Summer","Fall")

#We are now looping through a "matrix" instead of a 1-D vector

for(s in 1:ncol(seasons)){
  
  print(paste0("Working on season: ",colnames(seasons)[s]))
  
  sub_data = new_data[new_data$monthMST %in% seasons[,s],]
  
  hourly_dat = aggregate(sub_data$co2_enhan,by=list(sub_data$hourMST),FUN=mean, na.rm=TRUE)
  colnames(hourly_dat) <- c("hour","co2_enhancement")
  
  png(paste0("diurnal_co2_month_",colnames(seasons)[s],".png"), 600, 600,pointsize=24); par(mar=c(5,5,5,5))
  plot(hourly_dat$hour,hourly_dat$co2_enhancement,type="l",xlab="Time [MST]",ylab="CO2 [ppm]", ylim=c(0, 70), lwd=2,main=paste0("Season = ",colnames(seasons)[s]))
  dev.off()
  
}
