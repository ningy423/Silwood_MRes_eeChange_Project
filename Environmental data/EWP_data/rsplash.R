########## install rsplash ##########

install.packages("devtools",lib = "D:/Program Files (x86)/R-4.3.2/R-4.3.2/library")

library(devtools)

install_github( "dsval/rsplash")

library(rsplash)

# load some data
data(Bourne)
# run splash
run1<-splash.point(
  sw_in=Bourne$forcing$sw_in,	# shortwave radiation W/m2
  tc=Bourne$forcing$Ta,		# air temperature C
  pn= Bourne$forcing$P,		# precipitation mm
  lat=Bourne$md$latitude,		# latitude deg
  elev=Bourne$md$elev_m,		# elevation masl
  slop=Bourne$md$slop_250m,	# slope deg
  asp=Bourne$md$asp_250m,		# aspect deg
  soil_data=Bourne$soil, 		# soil data: sand,clay,som in w/w %. Gravel v/v %, bulk density g/cm3, and depth to the bedrock (m)**
  Au=Bourne$md$Aups_250m,		# upslope area m2
  resolution=250.0  			# resolution pixel dem used to get Au
)
#*NOTE: if slop=0.0 (flat surface) the lateral flow is assumed negligible, so: asp,Au and resolution can be ommitted, it won't affect the calculations since all the fluxes are assumed vertical.
#**Soil column thickness

# plot the snow water equivalent
plot(Bourne$forcing$swe,main='SWE (mm)');lines(run1$snow,col=2,lwd=2)
addLegend(legend.loc = "topright", legend.names =c('SWE obs.','SWE sim.'),col=c(1,2),lty=rep(1,2), lwd=rep(2,2))

#Compare the simulations of soil water content (mm) with the measurements taken up to Bourne$max_depth_sm (0.49 m):
# get the simulated water content in the measured region of the profile
swc<-unSWC(soil_data = Bourne$soil, uns_depth = Bourne$max_depth_sm,wn = run1$wn)

# plot the soil water content up to 0.49 m
dev.new()
plot(Bourne$forcing$sm,main=paste('SWC (mm)','up to',Bourne$max_depth_sm,'m'))
lines(swc[[1]],col=4,lwd=2)
addLegend(legend.loc = "topright", legend.names =c('SWC obs.','SWC sim.'),col=c(1,4),lty=rep(1,2), lwd=rep(2,2))

