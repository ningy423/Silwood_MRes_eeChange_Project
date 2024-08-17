
setwd("D:/ICL/Research Project/Data/Core Samples")
Oak <- read.csv(file = "Oak_Data_EC.csv") 


### Functional Geometric Relationship
###############################################################################

## regress Hm and a, with equation H = Hm*(1-exp(-a*D/Hm))

plot (Oak$D_m, Oak$H_m, xlab = "Diameter at Breast Height (m)", ylab = "Height (m)", xlim = c(0,0.6), ylim = c(0,32.5))

nonlinear_model <- nls(H ~ Hm*(1-exp(-a*D/Hm)), data = data.frame(D = Oak$D_m, H = Oak$H_m), start = list(a = 50, Hm = 30))

a <- coef(nonlinear_model)[["a"]]  # initial slope of height-diameter relationship

Hm <- coef(nonlinear_model)[["Hm"]]  # asymptotic maximum Oak height

curve(Hm*(1-exp(-a*x/Hm)), add = TRUE, col = "blue")


## regress c, with linear model between Ac and piDH/4a

piDH_4a <- (pi*Oak$D_m*Oak$H_m)/(4*a)

plot (piDH_4a, Oak$Ac_m2, xlab = "piDH/4a (m2)", ylab = "Crown Area (m2)", xlim = c(0, max(piDH_4a)), ylim = c(0, max(Oak$Ac)))

lm_model <- summary (lm(Oak$Ac ~ piDH_4a))

c <- lm_model$coefficients["piDH_4a","Estimate"]  # initial ratio of crown area to stem cross-sectional area

#std_error <- lm_model$coefficients["piDH_4a", "Std. Error"]

abline (lm(Oak$Ac ~ piDH_4a), col="blue")


## calculate fc

Oak $ fc <- Oak$H_m/(a*Oak$D_m)




### Carbon Allocation
###############################################################################

# load gpp and fapar data
# NOTE: the growing season is defined from August to July in next year considering the tree phenology relating to the wood formation
# NOTE: yearly GPP and fAPAR are summed and averaged from August in last year to July in current year
setwd("D:/ICL/Research Project/Data/Merge")
load("yearly_gpp_fapar.RData")
yearly_gpp_fapar$LAI <- log(1-yearly_gpp_fapar$Value.fAPAR....)/(-0.5)

# parameters and initialized condition

P <- yearly_gpp_fapar$Value.gpp  # actual gpp in kg C m-2
ρs <- 538   # sapwood density in kg C m-3
k <- 0.5   # PAR extinction coefficient
L <- yearly_gpp_fapar$LAI    # leaf area index in m2 m-2
y <- 0.6    # yield factor
ζ <- 0.17    # the ratio of fine-root mass to foliage area in kg C m-2
σ <- 14.72    # specific leaf area in m2 kg-1 C
rr <- 0.913   # specific respiration rate of fine-root in mol C mol-1 year-1
rs <- 0.0419   # specific respiration rate of sapwood in mol C mol-1 year-1
τf <- 1   # foliage turnover time in years
τr <- 1.27   # fine-root turnover time in years


# abstract years and order by decreasing
years <- sort (yearly_gpp_fapar$Year)


# initialize to store annual results
results <- list()


# looping through each year
for (year in years) {
  
  # get GPP and LAI for current year
  
  current_GPP <- P [yearly_gpp_fapar$Year == year]
  
  current_LAI <- L [yearly_gpp_fapar$Year == year]
  
  
  # calculate the amount of GPP allocated to each tree
  
  total_Ac <- sum(Oak$Ac_m2)
  
  Oak$gpp <- current_GPP*(Oak$Ac_m2/total_Ac)
  
  
  # initialize dD_dt, dWs_dt, dH_dt 
  
  dD_dt <- numeric(nrow(Oak))
  
  dWs_dt <- numeric(nrow(Oak))
  
  dH_dt <- numeric(nrow(Oak))
  
  
  # calculate dH_dD, dD_dt, dWs_dt, dH_dt
  
  dH_dD <- Oak$H_m / Oak$D_m
  
  A <- y * (Oak$gpp  - Oak$Ac_m2 * ρs * (1 - Oak$fc / 2) * (Oak$H_m * rs / c) - Oak$Ac_m2 * current_LAI * ζ * rr) - current_LAI * Oak$Ac_m2 * (1 / (σ * τf) + ζ / τr)
    
  B <- current_LAI * (pi * c / (4 * a)) * (a * Oak$D_m * (1 - Oak$H_m / Hm) + Oak$H_m) * (1 / σ + ζ) + (pi / 8) * ρs * (Oak$D_m^2 * dH_dD + 2 * Oak$D_m * Oak$H_m)
    
  dD_dt <- A/B
    
  dWs_dt <- (pi / 8) * ρs * (Oak$D_m^2 * dH_dD + 2 * Oak$D_m * Oak$H_m) * dD_dt
    
  dH_dt <- dD_dt*dH_dD
  
  
  # get new D, H, Ac, As in the previous year
    
  new_D_m <- Oak$D_m - dD_dt
    
  new_H_m <- Oak$H_m - dH_dt
  
  new_As_m2 <- new_D_m^2/4*pi
  
  ring_width_mm <- dD_dt/-2*1000
  
  
  # store the result
  
  results[[as.character(year)]] <- data.frame(
    ID = Oak$ID,
    PlotID = Oak$PlotID,
    Year = year,
    D_m = new_D_m,
    H_m = new_H_m,
    As_m2 = new_As_m2,
    fc = Oak$fc,
    GPP_kgCm2 = Oak$gpp,
    dD_dt = dD_dt,
    dH_dt = dH_dt,
    dWs_dt = dWs_dt,
    ring_width_mm = ring_width_mm
  )
  
  # renew D, H, As, Ac for the next loop
  
  Oak$D_m <- new_D_m
  
  Oak$H_m <- new_H_m
  
  Oak$As_m2 <- new_As_m2
  
}



# merge result of all years
final_results <- do.call (rbind,results)

final_results <- final_results [final_results$D_m > 0 & final_results$H_m > 0,]

# check the result   
plot(final_results$Year,final_results$ring_width_mm,col=as.factor(final_results$ID), xlab = "Year", ylab = "Ring Widths (mm)")
plot(final_results$ring_width_mm ~ as.factor(final_results$Year))
filtered_data <- subset(final_results, ID == "1242_2204_1111")
plot(filtered_data$Year, filtered_data$ring_width_mm, type = "o", col = "blue")


save (results, file = "Ring_Widths_Simulation.RData")
save (final_results, file = "Ring_Widths_Simulation_df.RData")


 
# looping through the last year to have a trial
   
#   current_GPP <- 0.8863839
#   total_Ac <- sum(Oak$Ac_m2)
#   Oak$gpp <- current_GPP*(Oak$Ac_m2/total_Ac)
 
#   dD_dt <- numeric(nrow(Oak))
#   dWs_dt <- numeric(nrow(Oak))
#   dH_dD <- Oak$H_m / Oak$D_m
   
#   A <- y * Oak$Ac_m2 * (Oak$gpp * (1 - exp(-k * L)) - ρs * (1 - Oak$fc / 2) * Oak$H_m * rs / c - L * ζ * rr) - L * Oak$Ac_m2 * (1 / (σ * τf) + ζ / τr)
#   B <- L * (pi * c / (4 * a)) * (a * Oak$D_m * (1 - Oak$H_m / Hm) + Oak$H_m) * (1 / σ + ζ) + (pi / 8) * ρs * (Oak$D_m^2 * dH_dD + 2 * Oak$D_m * Oak$H_m)
#   dD_dt <- A/B
#   dWs_dt <- (pi / 8) * ρs * (Oak$D_m^2 * dH_dD + 2 * Oak$D_m * Oak$H_m) * dD_dt
#   dH_dt <- dD_dt*dH_dD

