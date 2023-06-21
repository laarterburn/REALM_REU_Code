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

#rbind
Data<-rbind(Data2015LGN, Data2016LGN, Data2017LGN, Data2018LGN, Data2019LGN, Data2020LGN, Data2021LGN)

#Saving Time_UTC column as.POSIXct
Data$Time_UTC<-as.POSIXct(Data$Time_UTC, format="%Y-%m-%d%H:%M:%S",tz="GMT")

#Changing Time_UTC to MST
Time_MST <- Data$Time_UTC
print(Time_MST[1:5])
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
print(Time_MST[1:5])
Time_MST<-Time_MST[substr(Time_MST, 12, 13)%in%c(12:15)]

#selecting the 12:00-15:00 times
Data<-Data[substr(Data$Time_UTC, 12, 13)%in%c(12:15),]

#making baseline and quantile function
Baseline_Co2 = rollapply(Data$CO2_LGN,width=rwidth,FUN="quantile",p=.05,na.rm=T,fill=NA)
plot(Data$Time_UTC, Data$CO2_LGN, xlab="Year", ylab="CO2 [ppm]", main="5th Percentile Baseline of CO2 Emissions in Logan, Utah between 2015-2021 at 12:00-15:00 MST", type="l")

#plotting baseline
lines(Data$Time_UTC, Baseline_Co2, col="red")

#Merge data back together
new_data = data.frame(Data,Baseline_Co2,Time_MST)

#Taking the hour
hourMST = hour(new_data$Time_MST)
monthMST = month(new_data$Time_MST)
dayMST = day(new_data$Time_MST)
yearMST = year(new_data$Time_MST)

#Compute the CO2 enhancement by substracting out our "background"
co2_enhan = new_data$CO2_LGN - new_data$Baseline_Co2

#Merge more data together
new_data = data.frame(Data,Baseline_Co2,Time_MST,hourMST,monthMST,dayMST,co2_enhan)

#Loop through each month and make a diurnal plot of CO2 enhancements 
my_months = unique(monthMST)

#for(m in 1:length(my_months)){
  
  #sub_data = new_data[new_data$monthMST==my_months[m],]
  
  #daily_data = aggregate(sub_data$co2_enhan,by=list(sub_data$dayMST),FUN=mean, na.rm=TRUE)
  #colnames(daily_data) <- c("day","co2_enhancement")
  
  #png(paste0("diurnal_co2_month_",my_months[m],".png"), 600, 600,pointsize=24); par(mar=c(5,5,5,5))
 # plot(daily_data$day,daily_data$co2_enhancement,type="l",xlab="Time [MST]",ylab="CO2 Excess [ppm]", ylim=c(0, 70), lwd=2,main=paste0("Month =",my_months[m]))
  #dev.off()
  
}

#define our seasons... Based on meteorological seasons 

seasons = matrix(c(12,seq(1,11,by=1)),nrow=3,ncol=4)
colnames(seasons) <- c("Winter","Spring","Summer","Fall")

#We are now looping through a "matrix" instead of a 1-D vector

for(s in 1:ncol(seasons)){
  
  print(paste0("Working on season: ",colnames(seasons)[s]))
  
  sub_data = new_data[new_data$monthMST %in% seasons[,s],]
  
  daily_data = aggregate(new_data$co2_enhan,by=list(new_data$dayMST),FUN=mean, na.rm=TRUE)
  colnames(daily_data) <- c("day","co2_enhancement")
  
  png(paste0("diurnal_co2_month_",colnames(seasons)[s],".png"), 600, 600,pointsize=24); par(mar=c(5,5,5,5))
  plot(daily_data$day,daily_data$co2_enhancement,type="l",xlab="Days of Each Month Averaged",ylab="CO2 Excess [ppm]", ylim=c(0, 100), xlim=c(0, 12), lwd=2,main=paste0("Season = ",colnames(seasons)[s]))
  dev.off()
  
}



#Omit the NAs
days<-unique(substr(new_data$Time_MST,1,10))
averages<-NULL

for(d in days){
sub_dat<-new_data[substr(new_data$Time_MST,1,10)==d,]  
daily_avg<-mean(sub_dat$co2_enhan)  
averages<-rbind(averages,c(d,daily_avg))  
}

#working with averages
averages<-as.data.frame(averages)

DJF<-averages[substr(averages$V1,6,7)%in%c("12","01","02"),]
MAM<-averages[substr(averages$V1,6,7)%in%c("03","04","05"),]
JJA<-averages[substr(averages$V1,6,7)%in%c("06","07","08"),]
SON<-averages[substr(averages$V1,6,7)%in%c("09","10","11"),]

#grouping values by year for DJF
years<-unique(substr(new_data$Time_MST,1,4))
DJF_avg<-NULL

for(y in years){
sub_dat_2<-DJF[substr(DJF$V1,1,4)==y,]
year_avgs<-mean(as.numeric(as.character(sub_dat_2$V2)))  
DJF_avg<-rbind(DJF_avg,c(y,year_avgs))
  
}

#Grouping values by year for MAM
years<-unique(substr(new_data$Time_MST,1,4))
MAM_avg<-NULL

for(y in years){
  sub_dat_3<-MAM[substr(MAM$V1,1,4)==y,]
  year_avgs_3<-mean(as.numeric(as.character(sub_dat_3$V2)))  
  MAM_avg<-rbind(MAM_avg,c(y,year_avgs_3))
  
}

#Grouping values by year for JJA
years<-unique(substr(new_data$Time_MST,1,4))
JJA_avg<-NULL

for(y in years){
  sub_dat_4<-JJA[substr(JJA$V1,1,4)==y,]
  year_avgs_4<-mean(as.numeric(as.character(sub_dat_4$V2)))  
  JJA_avg<-rbind(JJA_avg,c(y,year_avgs_4))
  
}

#Grouping values by year for SON
years<-unique(substr(new_data$Time_MST,1,4))
SON_avg<-NULL

for(y in years){
  sub_dat_5<-SON[substr(SON$V1,1,4)==y,]
  year_avgs_5<-mean(as.numeric(as.character(sub_dat_5$V2)))  
  SON_avg<-rbind(SON_avg,c(y,year_avgs_5))
  
}

#Plotting data
plot(DJF_avg, type="l", main="DJF 12:00-15:00MST Yearly Averages", xlab = "Year", ylab = "CO2 Excess [PPM]", ylim= c(20,50))
plot(MAM_avg, type="l", main="MAM 12:00-15:00MST Yearly Averages", xlab = "Year", ylab = "CO2 Excess [PPM]", ylim = c(20,50))
plot(JJA_avg, type="l", main="JJA 12:00-15:00MST Yearly Averages", xlab = "Year", ylab = "CO2 Excess [PPM]", ylim = c(20,50))
plot(SON_avg, type="l", main="SON 12:00-15:00MST Yearly Averages", xlab = "Year", ylab = "CO2 Excess [PPM]", ylim = c(20,50))

#trendlines and legensds/ displaying p-value and slope of trendlines
DJF1<-as_tibble(DJF_avg)
DJF1$V1<-as.numeric(DJF1$V1)
DJF1$V2<-as.numeric(DJF1$V2)                    
lm_DJF<-lm(DJF1$V2 ~ DJF1$V1)
abline(lm_DJF, untf=FALSE, col="red")
xslopeSON <- signif(lm_SON$coef[[2]], 3)
xpSON <- signif(summary(lm_SON)$coef[2,4], 3)
legend(x="bottomright", y=2, legend=c(paste("P-value:",xpSON), paste("Slope:",xslopeSON)))

MAM1<-as_tibble(MAM_avg)
MAM1$V1<-as.numeric(MAM1$V1)
MAM1$V2<-as.numeric(MAM1$V2)
lm_MAM<-lm(MAM1$V2 ~ MAM1$V1)
abline(lm_MAM, untf=FALSE, col="red")
xslopeMAM <- signif(lm_MAM$coef[[2]], 3)
xpMAM <- signif(summary(lm_MAM)$coef[2,4], 3)
legend(x="topright", y=2, legend=c(paste("P-value:",xpMAM), paste("Slope:",xslopeMAM)))

JJA1<-as_tibble(JJA_avg)
JJA1$V1<-as.numeric(JJA1$V1)
JJA1$V2<-as.numeric(JJA1$V2)
lm_JJA<-lm(JJA1$V2 ~ JJA1$V1)
abline(lm_JJA, untf=FALSE, col="red")
xslopeJJA <- signif(lm_JJA$coef[[2]], 3)
xpJJA <- signif(summary(lm_JJA)$coef[2,4], 3)
legend(x="topright", y=2, legend=c(paste("P-value:",xpJJA), paste("Slope:",xslopeJJA)))

SON1<-as_tibble(SON_avg)
SON1$V1<-as.numeric(SON1$V1)
SON1$V2<-as.numeric(SON1$V2)
lm_SON<-lm(SON1$V2 ~ SON1$V1)
abline(lm_SON, untf=FALSE, col="red")
xslopeSON <- signif(lm_SON$coef[[2]], 3)
xpSON <- signif(summary(lm_SON)$coef[2,4], 3)
legend(x="topright", y=2, legend=c(paste("P-value:",xpSON), paste("Slope:",xslopeSON)))


