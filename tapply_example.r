#This R script reads in UUCON CO2 data for 2019 and create a dirunal average
#plot for select sites
#
#DVM  June 3rd 2021

path = "./"
site = "LGN"

#Read in our CO2 data as a csv file, which will save it as a "data frame" object
filename = paste0(path,"UUCON_CO2_CSP_DBK_FRU_HDP_HEB_HPL_IMC_LGN_LG2_ROO_RPK_SUG_SUN_WBB_hrly_2019.csv")
co2_dat = read.csv(filename,header=T,sep=",")

#Which station do we want to run this analysis for? Based on station that we sent within
#the 'site' variable further up.

co2_cname = paste0("CO2_",site)

#Average CO2 for each day within our 2019 file.
daily_co2 <- tapply(co2_dat[,co2_cname],list(co2_dat[,"Month"],co2_dat[,"Day"]),FUN=mean,na.rm=TRUE)

#Average CO2 across each hour of the day, for ALL days (results is a vector of numbers that corresponds to 
#each hour in the day)
diurnal_co2 <- tapply(co2_dat[,co2_cname],co2_dat[,"Hour"],FUN=mean,na.rm=TRUE)

print("Results for daily-averaged CO2 [ppm]")
print(daily_co2)

print("Results for diurnally-averaged CO2 [ppm]")
print(diurnal_co2)


