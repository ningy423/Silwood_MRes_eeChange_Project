#get soil moisture limitation
########################################################################

# 01.load the libraries
########################################################################
library(xts)
library(rsplash)
library(fastmatch)

########################################################################
### set the working directory
setwd("D:/ICL/Research Project/Data/Environmental data/Silwood_data_to_curate")


# 02.load the data
########################################################################

##load environmental
Silwood_forcing <- read.csv('Silwood_forcing_TEST_SPLASH.csv', stringsAsFactors = F)
#load soil data
Silwood_soil <- getSoilSite(lat=51.41156,lon=-0.64369, global_depth = NULL, set = "phys")
###this is from soilgrids. but probably Catalina knows where you can find this data. If not, soilgrids should be fine
Silwood_soil <- c(sand=22.485, clay=42.04, SOM=2.24, gravel=18.45, bd=1.34, depth=1)
#creat a time index
Silwood_forcing$date <- seq (as.Date('1987-01-01'), as.Date('2023-12-31'), by='month')



# 3. run splash
########################################################################
Silwood_splash <- splash.point (sw_in=Silwood_forcing$SW_IN_Wm2,		# shortwave radiation W/m2
	tc=Silwood_forcing$TA_C,		# air temperature C 
	pn= Silwood_forcing$PREC_mm,		# precipitation mm
	lat=51.414,				# latitude deg
	elev=112,					# elevation masl
	soil_data=Silwood_soil, # soil info sand, clay, som, gravel in %, bulk density in g/cm3 and depth in m
	time_index=Silwood_forcing$date,
	monthly_out=TRUE
)


# 4. save output and plot
########################################################################
save (Silwood_splash, file = 'Silwood_splash_trial.RData')
write.csv(Silwood_splash, file = 'Silwood_splash.csv')


# locale setting
# save original locale setting
original_locale <- Sys.getlocale("LC_TIME")

# set new locale and save as C format
Sys.setlocale("LC_TIME", "C")

plot (Silwood_splash[,1])
plot (Silwood_splash[,9])
plot(Silwood_splash[,1], xaxt = "n")

axis(1, at = index(Silwood_splash)[seq(1, length(index(Silwood_splash)), by = 12)], labels = format(index(Silwood_splash)[seq(1, length(index(Silwood_splash)), by = 12)], "%Y"))
abline(v = index(Silwood_splash)[seq(1, length(index(Silwood_splash)), by = 12)], col = "lightgray", lty = "dotted")

# recover original locale
Sys.setlocale("LC_TIME", original_locale)









