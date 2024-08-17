library(xts)
library(rpmodel)

# Calculate other variables need to model GPP
########################################################################


# 01.load the data
########################################################################
setwd("D:/ICL/Research Project/Data/Environmental data/Silwood_data_to_curate")
data <- read.csv("raw_data.csv", header = TRUE, sep = ",", fileEncoding = "windows-1252", quote = "\"", stringsAsFactors = FALSE, comment.char = "", na.strings = "")


# 02.create the time index
########################################################################
# Sys.setenv(TZ = "") if unifing time zone
ind<-strptime(data$DATE,format='%Y/%m/%d', tz = "GMT")


# 03. create the time series object
########################################################################
data_ts <-xts(data[,5:14],ind)


# 04. Get CO2 concentration (CO2) from 1987 to 2023 (ppm)
#########################################################################
CO2_maunaLoa<-read.table('ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt')
CO2_maunaLoa[CO2_maunaLoa==-99.99]<-NA
names(CO2_maunaLoa)<-c('year','month','decimal_date','average', 'interpolated','trend','#days')
# check
plot(CO2_maunaLoa$year,CO2_maunaLoa$average,type="l")
#subset to 1987-2023 and select monthly
CO2_maunaLoa<-CO2_maunaLoa[CO2_maunaLoa$year>=1987 & CO2_maunaLoa$year <=2023 ,]
data_ts$CO2<-CO2_maunaLoa$average #first simulation
data_ts$CO2<- 380 #second simulation with constant co2


# 05. Calculating Vapour Pressure Deficit (VPD) from 1987 to 2023 (Pa)
#########################################################################
# define parameters at Magnus Formula
A <- 0.6107
B <- 17.27
C <- 237.3
# calculate the saturation vapour pressure 
data_ts$es<-A*exp(B*data_ts$Avg.Temperature/(data_ts$Avg.Temperature+C))
#calculate the actual vapour pressure
data_ts$ea<-(data_ts$Relative.Air.Humidity..../100)* data_ts$es 
# calculate vpd (Pa): es - ea and convert to Pa
data_ts$vpd<-(data_ts$es - data_ts$ea)*1000.0


# 06. Calculate Photosynthetic Photon Flux Density (ppfd) from 1987 to 2023 (mol m-2 m-1)
##########################################################################
# calculate ppfd [mol/m2/month]:(1e-6)*(kfFEC*(1 - kalb_vis)*SW_monthly), 
# where kfFEC=2.04 from-photon flux-to-energy, umol/J (Meek et al., 1984), and kalb_vis = 0.03 visible light albedo (Sellers, 1985)
data_ts$ppfd <- (1e-6)*(2.04 *(1 - 0.03)*data_ts$Average.Solar.Radiation..W.m2. * 2592000)


# 07. Calculate mean alpha
###########################################################################
Silwood_splash <- read.csv("Silwood_splash.csv")
data_ts$alpha <- Silwood_splash$aet / Silwood_splash$pet


save(data_ts, file = "derived_data.RData")
write.csv(data_ts, "derived_data.csv")


# 08. vectorize pmodel
############################################################################
pmodel<-Vectorize(rpmodel,c("tc","vpd","co2","fapar","ppfd",'soilm','meanalpha'))             

# 09. run pmodel
############################################################################
#vectorize rpmodel (some unnecesary recycling of the vectors is happening there, were are working on the fix for the package, in the meantime, 'Vectorize' will save us)

pmodel_outputs<-rpmodel( 	tc = as.numeric(data_ts$Avg.Temperature), 
                          vpd = as.numeric(data_ts$vpd), 
                          co2 = as.numeric(data_ts$CO2),
                          fapar = as.numeric(data_ts$fAPAR....), 
                          ppfd = as.numeric(data_ts$ppfd), 
                          elv = 112,
                          meanalpha=as.numeric(data_ts$alpha),
                          soilm=as.numeric(data_ts$Soil.Moisture),
                          do_soilmstress = T)

pmodel_outputs_df <- as.data.frame(pmodel_outputs)

# 10. aggregate monthly GPP and annual GPP from result
###########################################################################
time_index <- index(data_ts)
monthly_pmodel <- xts (pmodel_outputs_df, order.by = time_index)
options(xts_check_TZ = FALSE)
annual_gpp <- apply.yearly (monthly_pmodel$gpp, sum)

setwd("D:/ICL/Research Project/Data/GPP")
save(pmodel_outputs, file = "pmodel.RData")
save(monthly_pmodel, file = "monthly_pmodel.RData")
save(annual_gpp, file = "annual_gpp.RData")


###########################################################################
#play with the outputs if you want, just for fun, repeat the previous line with any of the outputs, check rpmodel for details
###########################################################################
Sys.setlocale("LC_TIME", "C")
# set time zone as GMT
plot(pmodel_outputs$gpp)
plot(monthly_pmodel$gpp)
plot(annual_gpp)


library(ggplot2)
x_value <- as.POSIXct(index(monthly_pmodel))
x_value2 <- as.POSIXct(index(annual_gpp))

ggplot() + geom_line (data = as.data.frame(monthly_pmodel), aes(x= x_value, y = monthly_pmodel$gpp)) +
  labs(x="Year",y="GPP / (gC/m2)") + 
  scale_x_datetime(date_breaks = "2 year", date_labels = "%Y") +
  theme(axis.line = element_line(color = "black", size = 1)) 

ggplot() +
  geom_point(data = as.data.frame(annual_gpp), aes(x = x_value2, y = gpp)) +
  ylim (600,1010) +
  geom_line(data = as.data.frame(annual_gpp), aes(x = x_value2, y = gpp)) +
  labs(x="Year",y="GPP / (gC/m2)") +
  scale_x_datetime(date_breaks = "2 year", date_labels = "%Y") +
  theme(axis.line = element_line(color = "black", size = 1)) 



##############################################################################
# Question: is the actual vapour pressure calculated by es*(RH/100) or es*(1-RH/100)   
# Question: is the CO2 concentration the same as the CO2_maunal_oa$average
# Question: do I need to time 2592000 for calculating monthly PPFD as the solar irradiance is already monthly
