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
Data$Time_UTC<-as.POSIXlt(Data$Time_UTC, format="%Y-%m-%d %H:%M:%S",tz="GMT")

#Changing Time_UTC to MST
Time_MST <- Data$Time_UTC
print(Time_MST[1:5])
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
print(Time_MST[1:5])

#Selecting weekdays and organizing columns
Data$Time_UTC$wday
unique(Data$Time_UTC$wday)
Data<-cbind(Data,Data$Time_UTC$wday)
Dataweekdays<-Data %>% select(1:3) 
colnames(Dataweekdays)[colnames(Dataweekdays)=="Data$Time_UTC$wday"]<-"Days"

#organizing and combining weekend data
datasunday<-Dataweekdays[Dataweekdays$Days == "0",]
datasaturday<-Dataweekdays[Dataweekdays$Days == "6",]
dataweekend<-rbind(datasaturday, datasunday)

#organizing and combining weekday data
datamonday<-Dataweekdays[Dataweekdays$Days == "1",]
datatuesday<-Dataweekdays[Dataweekdays$Days == "2",]
datawednesday<-Dataweekdays[Dataweekdays$Days == "3",]
datathursday<-Dataweekdays[Dataweekdays$Days == "4",]
datafriday<-Dataweekdays[Dataweekdays$Days == "5",]
dataweekday<-rbind(datamonday, datatuesday, datawednesday, datathursday, datafriday)

#plotting regular weekend graph
plot(dataweekend$Time_UTC, dataweekend$CO2_LGN, type="l", xlab="Year", ylab="CO2 Emissions [PPM]", main ="Weekend CO2 Emissions in Logan, Utah 2015-2021")

#plotting regular weekday graph
plot(dataweekday$Time_UTC, dataweekday$CO2_LGN, type="l", xlab="Year", ylab="CO2 Emissions [PPM]", main ="Weekday CO2 Emissions in Logan, Utah 2015-2021")

#making weekday baseline and plotting
Baseline_Co2_weekday = rollapply(dataweekday$CO2_LGN,width=rwidth,FUN="quantile",p=.05,na.rm=T,fill=NA)
plot(dataweekday$Time_UTC, dataweekday$CO2_LGN, type="l", xlab="Year", ylab="CO2 Emissions [PPM]", main ="Weekday CO2 Emissions in Logan, Utah 2015-2021")
lines(dataweekday$Time_UTC, Baseline_Co2_weekday, col="red")
mergedweekday=data.frame(dataweekday,Baseline_Co2_weekday)

#making weekend baseline and plotting
Baseline_Co2_weekend = rollapply(dataweekend$CO2_LGN,width=rwidth,FUN="quantile",p=.05,na.rm=T,fill=NA)
plot(dataweekend$Time_UTC, dataweekend$CO2_LGN, type="l", xlab="Year", ylab="CO2 Emissions [PPM]", main ="Weekend CO2 Emissions in Logan, Utah 2015-2021")
lines(dataweekend$Time_UTC, Baseline_Co2_weekend, col="red")
mergedweekend=data.frame(dataweekend,Baseline_Co2_weekend)

#Compute the CO2 enhancement by substracting out our "background"
co2_enhan_weekday = mergedweekday$CO2_LGN - mergedweekday$Baseline_Co2_weekday
co2_enhan_weekend = mergedweekend$CO2_LGN - mergedweekend$Baseline_Co2_weekend

#selecting year MST
yearMSTwd = year(mergedweekday$Time_UTC)
yearMSTwe = year(mergedweekend$Time_UTC)

#adding month MST
monthMSTwd = month(mergedweekday$Time_UTC)
monthMSTwe = month(mergedweekend$Time_UTC)

#adding hour MST
hourMSTwd = hour(mergedweekday$Time_UTC)
hourMSTwe = hour(mergedweekend$Time_UTC)

#merge new data
mergedweekday=data.frame(dataweekday,Baseline_Co2_weekday, co2_enhan_weekday, yearMSTwd, monthMSTwd, hourMSTwd)
mergedweekend=data.frame(dataweekend,Baseline_Co2_weekend, co2_enhan_weekend, yearMSTwe, monthMSTwe, hourMSTwe)

#plotting CO2 enhancement for weekday (NOT averaged)
plot(mergedweekday$Time_UTC, mergedweekday$co2_enhan_weekday, type="l", xlab="Year", ylab = "CO2 Excess [PPM]", main="Weekday Excess CO2 Emissions in Logan, Utah between 2015-2021")

#plotting CO2 enhancement for weekend (NOT averaged)
plot(mergedweekend$Time_UTC, mergedweekend$co2_enhan_weekend, type="l", xlab="Year", ylab = "CO2 Excess [PPM]", main="Weekend Excess CO2 Emissions in Logan, Utah between 2015-2021", ylim=c(0,200))


#AVERAGED Co2 enhancement for weekday
mergeweekday1<-mergedweekday[mergedweekday[,"hourMSTwd"]%in% c(12:15),]
averageweekday=aggregate(mergeweekday1$co2_enhan_weekday,by=list(mergeweekday1$yearMSTwd),FUN=mean, na.rm=TRUE)
plot(averageweekday, type="l",xlab="Year", ylab = "CO2 Excess [PPM]", main="Weekday Excess CO2 Emissions from 12:00-15:00 in Logan, Utah between 2015-2021", ylim=c(0,60))


#AVERAGED Co2 enhancement for weekend
mergeweekend1<-mergedweekend[mergedweekend[,"hourMSTwd"]%in% c(12:15),]
averageweekend=aggregate(mergeweekend1$co2_enhan_weekend,by=list(mergeweekend1$yearMSTwe),FUN=mean, na.rm=TRUE)
plot(averageweekend, type="l",xlab="Year", ylab = "CO2 Excess [PPM]", main="Weekend Excess CO2 Emissions from 12:00-15:00 in Logan, Utah between 2015-2021", ylim=c(0,60))

#Weekend and weekday on one graph
plot(averageweekend, type="l",xlab="Year", ylab = "CO2 Excess [PPM]", main="Weekend & Weekday Excess CO2 between12:00-15:00 Emissions in Logan, Utah between 2015-2021", ylim=c(0,60), col="red")
points(averageweekday, type="l", col="black")
legend(x="bottomleft", y=2, legend=c("Weekend","Weekday"), col=c("red", "black"), pch = c("-","-"), title = "Time of Week")




#Extracting and averaging DJF for weekend
DJFtotalwe<-mergedweekend[mergedweekend[,"monthMSTwe"] %in% c(12,1,2),]
DJFtotalwe<-DJFtotalwe[DJFtotalwe[,"hourMSTwe"]%in% c(12:15),]
DJFtotalwe1<-aggregate(DJFtotalwe$co2_enhan_weekend,by=list(DJFtotalwe$yearMSTwe),FUN=mean, na.rm=TRUE)

#Extracting and averaging MAM for weekened
MAMtotalwe<-mergedweekend[mergedweekend[,"monthMSTwe"] %in% c(3,4,5),]
MAMtotalwe<-MAMtotalwe[MAMtotalwe[,"hourMSTwe"]%in% c(12:15),]
MAMtotalwe1<-aggregate(MAMtotalwe$co2_enhan_weekend,by=list(MAMtotalwe$yearMSTwe),FUN=mean, na.rm=TRUE)

#Extracting and averaging JJA for weekened
JJAtotalwe<-mergedweekend[mergedweekend[,"monthMSTwe"] %in% c(6,7,8),]
JJAtotalwe<-JJAtotalwe[JJAtotalwe[,"hourMSTwe"]%in% c(12:15),]
JJAtotalwe1<-aggregate(JJAtotalwe$co2_enhan_weekend,by=list(JJAtotalwe$yearMSTwe),FUN=mean, na.rm=TRUE)

#Extracting and averaging SON for weekened
SONtotalwe<-mergedweekend[mergedweekend[,"monthMSTwe"] %in% c(9,10,11),]
SONtotalwe<-SONtotalwe[SONtotalwe[,"hourMSTwe"]%in% c(12:15),]
SONtotalwe1<-aggregate(SONtotalwe$co2_enhan_weekend,by=list(SONtotalwe$yearMSTwe),FUN=mean, na.rm=TRUE)

#Extracting and averaging DJF for weekdays
DJFtotalwd<-mergedweekday[mergedweekday[,"monthMSTwd"] %in% c(12,1,2),]
DJFtotalwd<-DJFtotalwd[DJFtotalwd[,"hourMSTwd"]%in% c(12:15),]
DJFtotalwd1<-aggregate(DJFtotalwd$co2_enhan_weekday,by=list(DJFtotalwd$yearMSTwd),FUN=mean, na.rm=TRUE)

#Extracting and averaging MAM for weekdays
MAMtotalwd<-mergedweekday[mergedweekday[,"monthMSTwd"] %in% c(3,4,5),]
MAMtotalwd<-MAMtotalwd[MAMtotalwd[,"hourMSTwd"]%in% c(12:15),]
MAMtotalwd1<-aggregate(MAMtotalwd$co2_enhan_weekday,by=list(MAMtotalwd$yearMSTwd),FUN=mean, na.rm=TRUE)

#Extracting and averaging JJA for weekdays
JJAtotalwd<-mergedweekday[mergedweekday[,"monthMSTwd"] %in% c(6,7,8),]
JJAtotalwd<-JJAtotalwd[JJAtotalwd[,"hourMSTwd"]%in% c(12:15),]
JJAtotalwd1<-aggregate(JJAtotalwd$co2_enhan_weekday,by=list(JJAtotalwd$yearMSTwd),FUN=mean, na.rm=TRUE)

#Extracting and averaging JJA for weekdays
SONtotalwd<-mergedweekday[mergedweekday[,"monthMSTwd"] %in% c(9,10,11),]
SONtotalwd<-SONtotalwd[SONtotalwd[,"hourMSTwd"]%in% c(12:15),]
SONtotalwd1<-aggregate(SONtotalwd$co2_enhan_weekday,by=list(SONtotalwd$yearMSTwd),FUN=mean, na.rm=TRUE)

#DJF weekend vs weekday
plot(DJFtotalwe1, type="l",xlab="Year", ylab = "CO2 Excess [PPM]", main="DJF Weekend and Weekday Excess CO2 Emissions 12-15 ", ylim=c(0,60), col="red")
points(DJFtotalwd1, type="l",col="black")
legend(x="bottomleft", y=2, legend=c("Weekend","Weekday"), col=c("red", "black"), pch = c("-","-"), title = "Time of Week")

#MAM weekend vs weekday
plot(MAMtotalwe1, type="l",xlab="Year", ylab = "CO2 Excess [PPM]", main="MAM Weekend and Weekday Excess CO2 Emissions 12-15", ylim=c(0,60), col="red")
points(MAMtotalwd1, type="l",col="black")
legend(x="bottomleft", y=2, legend=c("Weekend","Weekday"), col=c("red", "black"), pch = c("-","-"), title = "Time of Week")

#JJA weekend vs weekday
plot(JJAtotalwe1, type="l",xlab="Year", ylab = "CO2 Excess [PPM]", main="JJA Weekend and Weekday Excess CO2 Emissions 12-15", ylim=c(0,60), col="red")
points(JJAtotalwd1, type="l",col="black")
legend(x="bottomleft", y=2, legend=c("Weekend","Weekday"), col=c("red", "black"), pch = c("-","-"), title = "Time of Week")

#SON weekend vs weekday
plot(SONtotalwe1, type="l",xlab="Year", ylab = "CO2 Excess [PPM]", main="SON Weekend and Weekday Excess CO2 Emissions 12-15 ", ylim=c(0,60), col="red")
points(SONtotalwd1, type="l",col="black")
legend(x="bottomleft", y=2, legend=c("Weekend","Weekday"), col=c("red", "black"), pch = c("-","-"), title = "Time of Week")




#DJF bar graph
totalDJFwe11<-mean(DJFtotalwe1$x,by=list(DJFtotalwe1$Group.1),FUN=mean, na.rm=TRUE)
totalDJFwd11<-mean(DJFtotalwd1$x, by=list(DJFtotalwd1$Group.1), FUN=mean, na.rm=TRUE)

#MAM bar graph
totalMAMwe11<-mean(MAMtotalwe1$x,by=list(MAMtotalwe1$Group.1),FUN=mean, na.rm=TRUE)
totalMAMwd11<-mean(MAMtotalwd1$x, by=list(MAMtotalwd1$Group.1), FUN=mean, na.rm=TRUE)

#JJA bar graph
totalJJAwe11<-mean(JJAtotalwe1$x,by=list(JJAtotalwe1$Group.1),FUN=mean, na.rm=TRUE)
totalJJAwd11<-mean(JJAtotalwd1$x, by=list(JJAtotalwd1$Group.1), FUN=mean, na.rm=TRUE)

#SON bar graph
totalSONwe11<-mean(SONtotalwe1$x,by=list(SONtotalwe1$Group.1),FUN=mean, na.rm=TRUE)
totalSONwd11<-mean(SONtotalwd1$x, by=list(SONtotalwd1$Group.1), FUN=mean, na.rm=TRUE)

#plotting bar graph(s)
trial<-c(totalDJFwe11, totalDJFwd11, totalMAMwe11, totalMAMwd11, totalJJAwe11, totalJJAwd11, totalSONwe11, totalSONwd11)
barplot(trial, names.arg= c("DJF", "DJF", "MAM", "MAM", "JJA", "JJA","SON", "SON"), ylab = "CO2 Excess [PPM]", main="Weekend and Weekday Excess CO2 Emissions 12:00-15:00 ", ylim=c(0,60), col=c("blue", "black","blue", "black","blue", "black","blue", "black"))
legend(x="topright", y=2, legend=c("Weekend","Weekday"), col=c("blue", "black"), pch = c("*","*"), title = "Time of Week")

