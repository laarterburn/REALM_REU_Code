
###Data for 2015###

#2015 JFM (NO DATA FOR SITE:ignore)
co2_LGN_2015<- Data2015 %>% select(1:5, 11)
co2_LGN_2015<- co2_LGN_2015[c(1:2160),]
co2_LGN_2015_ave<-tapply(co2_LGN_2015[,co2_LGN_2015_3], co2_LGN_2015$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2015_ave,type='l')

#2015 AMJ (NO DATA FOR SITE:ignore)
co2_LGN_2015_2<- Data2015 %>% select(1:5, 11)
co2_LGN_2015_2<- co2_LGN_2015_2[c(2161:4344),]
co2_LGN_2015_ave_2<-tapply(co2_LGN_2015_2$CO2, co2_LGN_2015_2$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2015_ave_2,type='l')

#2015 JAS
path="./"
site="LGN"

Data2015_3=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2015 (1).csv")
co2_dat_2015_3=read.csv(Data2015_3,header=T,sep=",")
co2_cname_2015_3 = paste0("CO2_",site)
co2_LGN_2015_3<- co2_dat_2015_3 %>% select(1:5, 11)
co2_LGN_2015_3[co2_LGN_2015_3[,"Month"] == 7|8|9,]
co2_LGN_2015_3$Time_UTC <- as.POSIXct(co2_LGN_2015_3$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2015_3$Time_UTC[1:5] 
Time_MST_3 <- co2_LGN_2015_3$Time_UTC
Time_MST_3[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST_3[1:5] 
co2_LGN_2015_ave_3<-tapply(co2_LGN_2015_3$CO2, co2_LGN_2015_3$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2015_ave_3,type='l',main="2015 JAS new format")

#2015 OND
path="./"
site="LGN"

Data2015_4=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2015 (1).csv")
co2_dat_2015_4=read.csv(Data2015_4,header=T,sep=",")
co2_cname_2015_4 = paste0("CO2_",site)
co2_LGN_2015_4<- co2_dat_2015_4 %>% select(1:5, 11)
co2_LGN_2015_4[co2_LGN_2015_4[,"Month"] == 10|11|12,]
co2_LGN_2015_4$Time_UTC <- as.POSIXct(co2_LGN_2015_4$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2015_4$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2015_ave_4<-tapply(co2_LGN_2015_4$CO2, co2_LGN_2015_4$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2015_ave_4,type='l', main="2015 OND new format")



###Data for 2016###

#2016 JFM
path="./"
site="LGN"

Data1_2016=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2016.csv")
co2_dat_2016=read.csv(Data1_2016,header=T,sep=",")
co2_cname_2016 = paste0("CO2_",site)
co2_LGN_2016<- co2_dat_2016 %>% select(1:5, 13)
co2_LGN_2016[co2_LGN_2016[,"Month"] == 1|2|3,]
co2_LGN_2016$Time_UTC <- as.POSIXct(co2_LGN_2016_2$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2016$Time_UTC[1:5] 
Time_MST <- co2_LGN_2016_2$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2016_ave<-tapply(co2_LGN_2016$CO2, co2_LGN_2016$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2016_ave,type='l')

#2016 AMJ
path="./"
site="LGN"

Data1_2016_2=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2016.csv")
co2_dat_2016_2=read.csv(Data1_2016_2,header=T,sep=",")
co2_cname_2016_2 = paste0("CO2_",site)
co2_LGN_2016_2<- co2_dat_2016_2 %>% select(1:5, 13)
co2_LGN_2016_2[co2_LGN_2016_2[,"Month"] == 4|5|6,]
co2_LGN_2016_2$Time_UTC <- as.POSIXct(co2_LGN_2016_2$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2016_2$Time_UTC[1:5] 
Time_MST <- co2_LGN_2016_2$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2016_ave2<-tapply(co2_LGN_2016_2$CO2, co2_LGN_2016_2$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2016_ave2, type='l')

#2016 JAS (NO DATA FOR SITE:ignore)
co2_LGN_2016_3<- Data2020 %>% select(1:5, 13)
co2_LGN_2016_3<- co2_LGN_2016_3[c(4370:6577),]
co2_LGN_2016_ave3<-tapply(co2_LGN_2016_3[,co2_LGN_2016_3], co2_LGN_2016_3$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2016_ave3, type='l')

#2016 OND (NO DATA FOR SITE:ignore)
co2_LGN_2016_4<- Data2020 %>% select(1:5, 13)
co2_LGN_2016_4<- co2_LGN_2016_4[c(6578:8784),]
co2_LGN_2016_ave4<-tapply(co2_LGN_2016_4$CO2, co2_LGN_2016_4$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2016_ave4, type='l')

###Data for 2017###

#2017 JFM
path="./"
site="LGN"
Data1_2017=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2017.csv")
co2_dat_2017=read.csv(Data1_2017,header=T,sep=",")
co2_cname_2017 = paste0("CO2_",site)
co2_LGN_2017<- co2_dat_2017 %>% select(1:5, 12)
co2_LGN_2017[co2_LGN_2017[,"Month"] == 1|2|3,]
co2_LGN_2017$Time_UTC <- as.POSIXct(co2_LGN_2017$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2017$Time_UTC[1:5] 
Time_MST <- co2_LGN_2017$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2017_ave<-tapply(co2_LGN_2017$CO2, co2_LGN_2017$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2017_ave,type='l')
                  
#2017 AMJ
path="./"
site="LGN"
Data1_2017=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2017.csv")
co2_dat_2017_2=read.csv(Data1_2017,header=T,sep=",")
co2_cname_2017 = paste0("CO2_",site)
co2_LGN_2017_2<- co2_dat_2017 %>% select(1:5, 12)
co2_LGN_2017_2[co2_LGN_2017_2[,"Month"] == 4|5|6,]
co2_LGN_2017_2$Time_UTC <- as.POSIXct(co2_LGN_2017_2$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2017_2$Time_UTC[1:5] 
Time_MST <- co2_LGN_2017_2$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2017_ave_2<-tapply(co2_LGN_2017_2$CO2, co2_LGN_2017_2$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2017_ave_2,type='l')
                  
#2017 JAS
path="./"
site="LGN"
Data1_2017=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2017.csv")
co2_dat_2017=read.csv(Data1_2017,header=T,sep=",")
co2_cname_2017_3 = paste0("CO2_",site)
co2_LGN_2017_3<- co2_dat_2017 %>% select(1:5, 12)
co2_LGN_2017_3[co2_LGN_2017_3[,"Month"] == 7|8|9,]
co2_LGN_2017_3$Time_UTC <- as.POSIXct(co2_LGN_2017_3$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2017_3$Time_UTC[1:5] 
Time_MST <- co2_LGN_2017_3$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2017_ave_3<-tapply(co2_LGN_2017_3$CO2, co2_LGN_2017_3$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2017_ave_3,type='l')
                  
#2017 OND
path="./"
site="LGN"
Data1_2017=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2017.csv")
co2_dat_2017=read.csv(Data1_2017,header=T,sep=",")
co2_cname_2017_4 = paste0("CO2_",site)
co2_LGN_2017_4<- co2_dat_2017 %>% select(1:5, 12)
co2_LGN_2017_4[co2_LGN_2017_4[,"Month"] == 10|11|12,]
co2_LGN_2017_4$Time_UTC <- as.POSIXct(co2_LGN_2017_4$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2017_4$Time_UTC[1:5] 
Time_MST <- co2_LGN_2017_4$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2017_ave_4<-tapply(co2_LGN_2017_4$CO2, co2_LGN_2017_4$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2017_ave_4,type='l')
                

###Data for 2018###

#2018 JFM
path="./"
site="LGN"

Data1_2018=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2018.csv")
co2_dat_2018=read.csv(Data1_2018,header=T,sep=",")
co2_cname_2018 = paste0("CO2_",site)
co2_LGN_2018<- co2_dat_2018 %>% select(1:5, 12)
co2_LGN_2018[co2_LGN_2018[,"Month"] == 1|2|3,]
co2_LGN_2018$Time_UTC <- as.POSIXct(co2_LGN_2018$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2018$Time_UTC[1:5] 
Time_MST <- co2_LGN_2018$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2018_ave<-tapply(co2_LGN_2018$CO2, co2_LGN_2018$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2018_ave,type='l')

#2018 AMJ
path="./"
site="LGN"

Data1_2018=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2018.csv")
co2_dat_2018=read.csv(Data1_2018,header=T,sep=",")
co2_cname_2018 = paste0("CO2_",site)
co2_LGN_2018_2<- co2_dat_2018 %>% select(1:5, 12)
co2_LGN_2018_2[co2_LGN_2018_2[,"Month"] == 4|5|6,]
co2_LGN_2018_2$Time_UTC <- as.POSIXct(co2_LGN_2018_2$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2018_2$Time_UTC[1:5] 
Time_MST <- co2_LGN_2018_2$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2018_ave_2<-tapply(co2_LGN_2018_2$CO2, co2_LGN_2018_2$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2018_ave_2,type='l')

#2018 JAS
path="./"
site="LGN"

Data1_2018=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2018.csv")
co2_dat_2018=read.csv(Data1_2018,header=T,sep=",")
co2_cname_2018 = paste0("CO2_",site)
co2_LGN_2018_3<- co2_dat_2018 %>% select(1:5, 12)
co2_LGN_2018_3[co2_LGN_2018_3[,"Month"] == 7|8|9,]
co2_LGN_2018_3$Time_UTC <- as.POSIXct(co2_LGN_2018_3$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2018_3$Time_UTC[1:5] 
Time_MST <- co2_LGN_2018_3$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2018_ave_3<-tapply(co2_LGN_2018_3$CO2, co2_LGN_2018_3$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2018_ave_3,type='l')

#2018 OND
path="./"
site="LGN"

Data1_2018=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2018.csv")
co2_dat_2018=read.csv(Data1_2018,header=T,sep=",")
co2_cname_2018 = paste0("CO2_",site)
co2_LGN_2018_4<- co2_dat_2018 %>% select(1:5, 12)
co2_LGN_2018_4[co2_LGN_2018_4[,"Month"] == 10|11|12,]
co2_LGN_2018_4$Time_UTC <- as.POSIXct(co2_LGN_2018_4$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2018_4$Time_UTC[1:5] 
Time_MST <- co2_LGN_2018_4$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2018_ave_4<-tapply(co2_LGN_2018_4$CO2, co2_LGN_2018_4$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2018_ave_4,type='l')



###Data for 2019###

#2019 JFM
path="./"
site="LGN"

Data1_2019=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2019 (2).csv")
co2_dat_2019=read.csv(Data1_2019,header=T,sep=",")
co2_cname_2019 = paste0("CO2_",site)
co2_LGN_2019<- co2_dat_2019 %>% select(1:5, 13)
co2_LGN_2019[co2_LGN_2019[,"Month"] == 1|2|3,]
co2_LGN_2019$Time_UTC <- as.POSIXct(co2_LGN_2019$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2019$Time_UTC[1:5] 
Time_MST <- co2_LGN_2019$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2019_ave<-tapply(co2_LGN_2019$CO2, co2_LGN_2019$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2019_ave,type='l')

#2019 AMJ
path="./"
site="LGN"

Data1_2019=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2019 (2).csv")
co2_dat_2019=read.csv(Data1_2019,header=T,sep=",")
co2_cname_2019 = paste0("CO2_",site)
co2_LGN_2019_2<- co2_dat_2019 %>% select(1:5, 13)
co2_LGN_2019_2[co2_LGN_2019_2[,"Month"] == 4|5|6,]
co2_LGN_2019_2$Time_UTC <- as.POSIXct(co2_LGN_2019_2$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2019_2$Time_UTC[1:5] 
Time_MST <- co2_LGN_2019_2$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2019_ave_2<-tapply(co2_LGN_2019_2$CO2, co2_LGN_2019_2$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2019_ave_2,type='l')

#2019 JAS
path="./"
site="LGN"

Data1_2019=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2019 (2).csv")
co2_dat_2019=read.csv(Data1_2019,header=T,sep=",")
co2_cname_2019 = paste0("CO2_",site)
co2_LGN_2019_3<- co2_dat_2019 %>% select(1:5, 13)
co2_LGN_2019_3[co2_LGN_2019_3[,"Month"] == 7|8|9,]
co2_LGN_2019_3$Time_UTC <- as.POSIXct(co2_LGN_2019_3$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2019_3$Time_UTC[1:5] 
Time_MST <- co2_LGN_2019_3$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2019_ave_3<-tapply(co2_LGN_2019_3$CO2, co2_LGN_2019_3$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2019_ave_3,type='l')

#2019 OND
path="./"
site="LGN"

Data1_2019=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2019 (2).csv")
co2_dat_2019=read.csv(Data1_2019,header=T,sep=",")
co2_cname_2019 = paste0("CO2_",site)
co2_LGN_2019_4<- co2_dat_2019 %>% select(1:5, 13)
co2_LGN_2019_4[co2_LGN_2019_4[,"Month"] == 10|11|12,]
co2_LGN_2019_4$Time_UTC <- as.POSIXct(co2_LGN_2019_4$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2019_4$Time_UTC[1:5] 
Time_MST <- co2_LGN_2019_4$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2019_ave_4<-tapply(co2_LGN_2019_4$CO2, co2_LGN_2019_4$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2019_ave_4,type='l')

###Data for 2020####

#2020 JFM
path="./"
site="LGN"

Data1_2020=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2020 (1).csv")
co2_dat_2020=read.csv(Data1_2020,header=T,sep=",")
co2_cname_2020 = paste0("CO2_",site)
co2_LGN_2020<- co2_dat_2020 %>% select(1:5, 13)
co2_LGN_2020[co2_LGN_2020[,"Month"] == 1|2|3,]
co2_LGN_2020$Time_UTC <- as.POSIXct(co2_LGN_2020$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2020$Time_UTC[1:5] 
Time_MST <- co2_LGN_2020$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2020_ave<-tapply(co2_LGN_2020$CO2, co2_LGN_2020$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2020_ave,type='l')

#2020 AMJ (co2_LG2)
path="./"
site="LGN"

Data1_2020=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2020 (1).csv")
co2_dat_2020=read.csv(Data1_2020,header=T,sep=",")
co2_cname_2020 = paste0("CO2_",site)
co2_LGN_2020_2<- co2_dat_2020 %>% select(1:5, 14)
co2_LGN_2020_2[co2_LGN_2020_2[,"Month"] == 4|5|6,]
co2_LGN_2020_2$Time_UTC <- as.POSIXct(co2_LGN_2020_2$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2020_2$Time_UTC[1:5] 
Time_MST <- co2_LGN_2020_2$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2020_ave2<-tapply(co2_LGN_2020_2$CO2, co2_LGN_2020_2$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2020_ave2, type='l')

#2020 JAS (co2_LG2)
path="./"
site="LGN"

Data1_2020=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2020 (1).csv")
co2_dat_2020=read.csv(Data1_2020,header=T,sep=",")
co2_cname_2020 = paste0("CO2_",site)
co2_LGN_2020_3<- co2_dat_2020 %>% select(1:5, 14)
co2_LGN_2020_3[co2_LGN_2020_3[,"Month"] == 7|8|9,]
co2_LGN_2020_3$Time_UTC <- as.POSIXct(co2_LGN_2020_3$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2020_3$Time_UTC[1:5] 
Time_MST <- co2_LGN_2020_3$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2020_ave3<-tapply(co2_LGN_2020_3$CO2, co2_LGN_2020_3$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2020_ave3, type='l')

#2020 OND (co2_LG2)
path="./"
site="LGN"

Data1_2020=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2020 (1).csv")
co2_dat_2020=read.csv(Data1_2020,header=T,sep=",")
co2_cname_2020 = paste0("CO2_",site)
co2_LGN_2020_4<- co2_dat_2020 %>% select(1:5, 14)
co2_LGN_2020_4[co2_LGN_2020_4[,"Month"] == 10|11|12,]
co2_LGN_2020_4$Time_UTC <- as.POSIXct(co2_LGN_2020_4$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2020_4$Time_UTC[1:5] 
Time_MST <- co2_LGN_2020_4$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2020_ave4<-tapply(co2_LGN_2020_4$CO2, co2_LGN_2020_4$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2020_ave4, type='l')

###Data for 2021###

#2021 JFM (co2_LG2)
path="./"
site="LGN"

Data1_2021=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2021 (1).csv")
co2_dat_2021=read.csv(Data1_2021,header=T,sep=",")
co2_cname_2021 = paste0("CO2_",site)
co2_LGN_2021<- co2_dat_2021 %>% select(1:5, 12)
co2_LGN_2021[co2_LGN_2021[,"Month"] == 1|2|3,]
co2_LGN_2021$Time_UTC <- as.POSIXct(co2_LGN_2021$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2021$Time_UTC[1:5] 
Time_MST <- co2_LGN_2021$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2021_ave<-tapply(co2_LGN_2021$CO2, co2_LGN_2021$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2021_ave,type='l')

#2021 April (only for co2_LG2)
path="./"
site="LGN"

Data1_2021=paste0(path, "UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2021 (1).csv")
                      co2_dat_2021=read.csv(Data1_2021,header=T,sep=",")
co2_cname_2021 = paste0("CO2_",site)
co2_LGN_2021_2<- co2_dat_2021 %>% select(1:5, 12)
co2_LGN_2021_2[co2_LGN_2021_2[,"Month"] == 4|5|6,]
co2_LGN_2021_2$Time_UTC <- as.POSIXct(co2_LGN_2021_2$Time_UTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")
co2_LGN_2021_2$Time_UTC[1:5] 
Time_MST <- co2_LGN_2021_2$Time_UTC
Time_MST[1:5]
attributes(Time_MST)$tzone
attributes(Time_MST)$tzone <- "MST"
Time_MST[1:5] 
co2_LGN_2021_ave_2<-tapply(co2_LGN_2021_2$CO2, co2_LGN_2021_2$Hour, mean, na.rm=TRUE)
plot(co2_LGN_2021_ave_2,type='l')

#Graph for JFM
HOD<- names(co2_LGN_2015_ave)

plot(y=co2_LGN_2015_ave,x=HOD,type='l',ylab="CO2 [PPM]",xlab="Hour of Day [MST]",ylim=c(400,500),col="red", main="CO2 Emissions for Logan, Utah January-March",)
points(y=co2_LGN_2016_ave,x=HOD,col="orange",type="l")
points(y=co2_LGN_2017_ave,x=HOD,col="yellow",type="l")
points(y=co2_LGN_2018_ave,x=HOD,col="green",type="l")
points(y=co2_LGN_2019_ave,x=HOD,col="blue",type="l")
points(y=co2_LGN_2020_ave,x=HOD,col="purple",type="l")
points(y=co2_LGN_2021_ave,x=HOD,col="black",type="l")

legend(x="topright", y=2, legend=c("2015","2016","2017","2018","2019","2020","2021"), col=c("red", "orange", "yellow", "green", "blue", "purple", "black"), pch = c("-","-","-","-","-","-"), title = "Year")

#Graphs for AMJ
HOD<- names(co2_LGN_2015_ave_2)

plot(y=co2_LGN_2015_ave_2,x=HOD,type='l',ylab="CO2 [PPM]",xlab="Hour of Day [MST]",ylim=c(400,500),col="red", main="CO2 Emissions for Logan, Utah April-June",)
points(y=co2_LGN_2016_ave2,x=HOD,col="orange",type="l")
points(y=co2_LGN_2017_ave_2,x=HOD,col="yellow",type="l")
points(y=co2_LGN_2018_ave_2,x=HOD,col="green",type="l")
points(y=co2_LGN_2019_ave_2,x=HOD,col="blue",type="l")
points(y=co2_LGN_2020_ave_2,x=HOD,col="purple",type="l")
points(y=co2_LGN_2021_ave_2,x=HOD,col="black",type="l")

legend(x="topright", y=2, legend=c("2015","2016","2017","2018","2019","2020","2021"), col=c("red", "orange", "yellow", "green", "blue", "purple", "black"), pch = c("-","-","-","-","-","-"), title = "Year")

#Graphs for JAS
HOD<- names(co2_LGN_2015_ave_3)

plot(y=co2_LGN_2015_ave_3,x=HOD,type='l',ylab="CO2 [PPM]",xlab="Hour of Day [MST]",ylim=c(400,500),col="red", main="CO2 Emissions for Logan, Utah in July-September",)
points(y=co2_LGN_2016_ave3,x=HOD,col="orange",type="l")
points(y=co2_LGN_2017_ave_3,x=HOD,col="yellow",type="l")
points(y=co2_LGN_2018_ave_3,x=HOD,col="green",type="l")
points(y=co2_LGN_2019_ave_3,x=HOD,col="blue",type="l")
points(y=co2_LGN_2020_ave3,x=HOD,col="purple",type="l")
#no data for 2021 JAS

legend(x="topright", y=2, legend=c("2015","2016","2017","2018","2019","2020","2021"), col=c("red", "orange", "yellow", "green", "blue", "purple", "black"), pch = c("-","-","-","-","-","-"), title = "Year")

#Graphs for OND
HOD<- names(co2_LGN_2015_ave_4)

plot(y=co2_LGN_2015_ave_4,x=HOD,type='l',ylab="CO2 [PPM]",xlab="Hour of Day [MST]",ylim=c(400,500),col="red", main="CO2 Emissions for Logan, Utah October-December",)
points(y=co2_LGN_2016_ave4,x=HOD,col="orange",type="l")
points(y=co2_LGN_2017_ave_4,x=HOD,col="yellow",type="l")
points(y=co2_LGN_2018_ave_4,x=HOD,col="green",type="l")
points(y=co2_LGN_2019_ave_4,x=HOD,col="blue",type="l")
points(y=co2_LGN_2020_ave4,x=HOD,col="purple",type="l")
#no data for 2021 JAS

legend(x="topright", y=2, legend=c("2015","2016","2017","2018","2019","2020","2021"), col=c("red", "orange", "yellow", "green", "blue", "purple", "black"), pch = c("-","-","-","-","-","-"), title = "Year")

