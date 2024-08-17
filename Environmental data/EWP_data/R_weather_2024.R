# code to calculate monthly average weather data

rm(list=ls()) # clear objects in workspace
graphics.off() #close all open figures and graphics objects
setwd("D:/ICL/Research Project/Data/Environmental data/EWP_data") #set working directory


# to calculate mean monthly averages for Max, Min temperature and rainfall from 2010 to last completed year

DataIn<- read.csv("SilwoodWeatherDaily.csv", header = TRUE, stringsAsFactors = F) 
# from January 2010 to present finish month --(Jan 2024)
# it uses file for daily measures
str(DataIn)
library(data.table)
DataIn$TIMESTAMP<-as.Date(DataIn$TIMESTAMP, "%d/%m/%Y") # because my data has the hole year, four numbers, I need to use capital Y, if two numbers then small "y“05/27/84” is in the format %m/%d/%y, while “May 27 1984” is in the format %B %d %Y.
#now I will calculate at once the mean for three columns min, max temperature and rain
cols=c("Air_Temp_Max...Deg.C...Max.", "Air_Temp_Min...Deg.C...Min.", "Soil_Temp_2in...Deg.C...Smp.", "Soil_Temp_4in...Deg.C...Smp.") #first I create a vector with the name of the columns I will use to calculate
DataOut<-setDT(DataIn)[,lapply (.SD, mean), by=.(year(TIMESTAMP), month(TIMESTAMP)), .SDcols=cols] #lapply is used
# #to calculate the mean for a subset of data table (.SD) by year and month, using only columns (.SDCols=)call
# #in vector "cols"
DataOut2<-setDT(DataIn)[,sum(Rain_mm_Tot...mm...Tot.), by=.(year(TIMESTAMP), month(TIMESTAMP))] 
# sum the rain by month and year. I have to create another out data because it create a three column
# data.frame, not only one column
write.csv(DataOut,"DataOut.csv") #delete row for December 2009 that has incomplete data
write.csv(DataOut2,"DataOut2.csv") #delete row for December 2009 that has incomplete data



# to calculate average from each measure since 1986 to last completed year

DataIn<- read.csv("SilwoodWeatherMonthly87_23.csv", header = TRUE)#check structure  ????????????????????????????????
str(DataIn)
cols=c("AirTempMax_oC", "AirTempMin_oC", "SoilTemp_2in_oC", "SoilTemp_4in_oC", "TotalMonthRain_mm") #first I create a vector with the name of the columns I will use to calculate
DataOut3<-setDT(DataIn)[,lapply (.SD, mean), by=.(Month), .SDcols=cols]
write.csv(DataOut3,"DataOut3.csv")


#Code to calculate monthly averages for other parameters from hourly measurement (Air humidity, radiation, wind speed)
DataIn<- read.csv("SilwoodWeatherHourly.csv", header = TRUE, stringsAsFactors = F)
str(DataIn)

library(data.table)
DataIn$TIMESTAMP<-as.Date(DataIn$TIMESTAMP, "%d/%m/%Y") # becouse my data has the ehole year four numbers I need to use capital Y, if two numbers then small y“05/27/84” is in the format %m/%d/%y, while “May 27 1984” is in the format %B %d %Y.
#now I will calculate at once the mean for three columns min, max temperature and rain
cols=c("RH......Smp.", "SlrW_Avg..W.m.2...Avg.","SlrMJ_Tot..MJ.m.2...Tot.", "WS_kph_..kilometers.hour...Avg.") #first I create a vector with the name of the columns I will use to calculate
DataOut4<-setDT(DataIn)[,lapply (.SD, mean), by=.(year(TIMESTAMP), month(TIMESTAMP)), .SDcols=cols] #lapply is used
write.csv(DataOut4,"DataOut4.csv")

