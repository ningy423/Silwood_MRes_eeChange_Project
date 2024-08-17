
setwd("D:/ICL/Research Project/Data/Core Samples")
Oak <- read.csv(file = "Oak_Data_EC.csv") 


### Functional Geometric Relationship
###############################################################################

## regress Hm and a, with equation H = Hm*(1-exp(-a*D/Hm))

plot (Oak$D_m, Oak$H_m, xlab = "Diameter at Breast Height (m)", ylab = "Height (m)", xlim = c(0,max(Oak$D_m)), ylim = c(0,max(Oak$H_m)))

nonlinear_model <- nls(H ~ Hm*(1-exp(-a*D/Hm)), data = data.frame(D = Oak$D_m, H = Oak$H_m), start = list(a = 50, Hm = 30))

a <- coef(nonlinear_model)[["a"]]  # initial slope of height-diameter relationship

Hm <- coef(nonlinear_model)[["Hm"]]  # asymptotic maximum Oak height

curve(Hm*(1-exp(-a*x/Hm)), add = TRUE, col = "blue")


## regress c, with linear model between Ac and piDH/4a

piDH_4a <- (pi*Oak$D_m*Oak$H_m)/(4*a)

plot (piDH_4a, Oak$Ac_m2, xlab = "piDH/4a (m2)", ylab = "Crown Area (m2)", xlim = c(0, max(piDH_4a)), ylim = c(0, max(Oak$Ac)))

lm_model <- lm(Oak$Ac_m2 ~ piDH_4a - 1)

summary(lm_model)

c <- coef(lm_model)[["piDH_4a"]]  # initial ratio of crown area to stem cross-sectional area

#std_error <- lm_model$coefficients["piDH_4a", "Std. Error"]

abline (lm_model, col="blue")


## calculate fc

Oak $ fc <- Oak$H_m/(a*Oak$D_m)



### Carbon Allocation
###############################################################################

# load gpp and lai data
# NOTE: the growing season is defined from April to October considering the tree phenology relating to the wood formation
# NOTE: yearly gpp and fAPAR are averaged from April to October
setwd("D:/ICL/Research Project/Data/GPP")
load("yearly_gpp_df.RData")
setwd("D:/ICL/Research Project/Data/Environmental data/Silwood_data_to_curate")
load("environment_df.RData")

# parameters and initialized condition

P <- yearly_gpp_df$gpp  # actual gpp in kg C m-2
L <- environment_df$LAI    # leaf area index in m2 m-2
ρs <- 538   # sapwood density in kg C m-3
k <- 0.5   # PAR extinction coefficient
y <- 0.6    # yield factor
ζ <- 0.17    # the ratio of fine-root mass to foliage area in kg C m-2
σ <- 25.08    # specific leaf area in m2 kg-1 
rr <- 0.913   # specific respiration rate of fine-root in mol C mol-1 year-1
rs <- 0.0419   # specific respiration rate of sapwood in mol C mol-1 year-1
τf <- 1.0   # foliage turnover time in years
τr <- 1.27   # fine-root turnover time in years

# abstract years and order by decreasing
years <- sort (yearly_gpp_df$Year, decreasing = T)
trees <- Oak

# define objective function
objective_function <- function(x, current_D, current_H, current_Ac, Hm, a, y, tree, current_GPP, current_LAI, c, rs, rr, ζ, σ, τf, τr, ρs) {
  # x as previous year's diameter
  previous_D <- x
  previous_H <- Hm * (1 - exp(-a * previous_D / Hm))
  dH_dD <- a * exp(-a * previous_D / Hm)
  dD_dt <- current_D - previous_D
  dH_dt <- dD_dt*dH_dD
  dAc_dt <- (pi*c/(4*a))*(dD_dt*previous_H + dH_dt*previous_D)
  previous_Ac <- current_Ac - dAc_dt
  previous_fc <- previous_H/(a*previous_D)
  A <- y * previous_Ac * (current_GPP - ρs * (1 - previous_fc / 2) * current_H * rs / c - current_LAI * ζ * rr) - current_LAI * previous_Ac * (1 / (σ * τf) + ζ / τr)
  B <- current_LAI * (pi * c / (4 * a)) * (a * previous_D * (1 - previous_H / Hm) + previous_H) * (1 / σ + ζ) + (pi / 8) * ρs * (previous_D^2 * dH_dD + 2 * previous_D * previous_H)
  return(abs(A / B - dD_dt))
}

# initialize to store annual results
results <- list()

# looping through each year
for (year in years) {

  # get GPP and LAI for current year
  
  current_GPP <- P [yearly_gpp_df$Year == year]
  
  current_LAI <- L [environment_df$Year == year]
  
  # calculate the amount of carbon allocated to each tree
  
  trees$gpp <- current_GPP * trees$Ac_m2 
  
  # initialize dD_dt, dWs_dt, dH_dt 
  
  dD_dt <- numeric(nrow(trees))
  
  dWs_dt <- numeric(nrow(trees))
  
  dH_dt <- numeric(nrow(trees))
  
  dAc_dt <- numeric(nrow(trees))
  
  ring_width_mm <- numeric(nrow(trees))
  
  
  # calculate dH_dD, dD_dt, dWs_dt, dH_dt, dAc_dt
  
  for (i in 1:nrow(trees)) {
   tree <- trees[i, ]
  
  current_D <- tree$D_m
  
  current_H <- tree$H_m
  
  current_Ac <- tree$Ac_m2
  
  previous_D <- optim(par = current_D, 
                      fn = objective_function, 
                      current_D = current_D, 
                      current_H = current_H, 
                      current_Ac = current_Ac,
                      current_GPP = current_GPP,
                      Hm = Hm, 
                      a = a, 
                      y = y, 
                      tree = tree, 
                      current_LAI = current_LAI, 
                      c = c, 
                      rs = rs, 
                      rr = rr, 
                      ζ = ζ, 
                      σ = σ, 
                      τf = τf, 
                      τr = τr, 
                      ρs = ρs)$par
  
  previous_H <- Hm * (1 - exp(-a * previous_D / Hm))
  
  dH_dD <- a * exp(-a*previous_D/Hm)  #NOTE: using the equation a*exp(-a*D/Hm) and a*(1-H/Hm) will produce different results
  
  dD_dt[i] <- current_D - previous_D
  
  ring_width_mm[i] <- (dD_dt[i]/-2)*1000
  
  ring_width_mm <- ifelse (ring_width_mm < 0, NA, ring_width_mm) 
    
  dWs_dt[i] <- (pi / 8) * ρs * (previous_D^2 * dH_dD + 2 * previous_D * previous_H) * dD_dt[i]
    
  dH_dt[i] <- dD_dt[i]*dH_dD
  
  dAc_dt[i] <- (pi*c/(4*a))*(dD_dt[i]*previous_H + dH_dt[i]*previous_D)
  
  previous_Ac <- current_Ac + dAc_dt
  
  previous_fc <- previous_H/(a*previous_D)
  
  }
  
  
  # store the result
  
  results[[as.character(year)]] <- data.frame(
    Year = year,
    ID = trees$ID,
    PlotID = trees$PlotID,
    GPP_kgC = trees$gpp,
    ring_width_mm = ring_width_mm,
    H_m = trees$H_m,
    D_m = trees$D_m,
    Ac_m2 = trees$Ac_m2,
    fc = trees$fc,
    dD_dt = dD_dt,
    dH_dt = dH_dt,
    dAc_dt = dAc_dt,
    dWs_dt = dWs_dt
  )
  
  
  # get new D, H, Ac, As in the previous year
  
  new_D_m <- trees$D_m - dD_dt
  
  new_H_m <- trees$H_m - dH_dt
  
  new_Ac_m2 <- trees$Ac_m2 - dAc_dt
  
  new_fc <- new_H_m/(a*new_D_m)
  
  
  # renew D, H, Ac, fc for the next loop
  
  trees$D_m <- new_D_m
  
  trees$H_m <- new_H_m
  
  trees$Ac_m2 <- new_Ac_m2
  
  trees$fc <- new_fc
  
}



# merge result of all years
final_results <- do.call (rbind,results)

# final_results <- final_results [final_results$ring_width_mm > 0,]



# check the result   
# ring_widths_simulation_31 is the result with σ=14.72, 
# ring_widths_simulation_32 is the result with σ=25.08 and Ac iteration dAc_dt <- (pi*c/(4*a))*dD_dt*dH_dt
# ring_widths_simulation_33 is the result with σ=25.08, Ac iteration and fc iteration of new_fc <- new_H_m/(a*new_D_m)  
# ring_widths_simulation_34 is the result with the assumpation previous_D + dD_dt = current_D, and Ac and fc iteration  ### the most accurate one so far

plot(final_results$Year,final_results$ring_width_mm,col=as.factor(final_results$ID), xlab = "Year", ylab = "Ring Widths (mm)")
plot(final_results$ring_width_mm ~ as.factor(final_results$Year), xlab = "Year", ylab = "Ring Widths (mm)")
filtered_data <- subset(final_results, ID == "1223_2180_1111")
plot(filtered_data$Year, filtered_data$ring_width_mm, type = "o", col = "blue")

plot(final_results$D_m,final_results$H_m, col=as.factor(final_results$ID), xlim = c(0,max(final_results$D_m)), ylim = c(0,max(final_results$H_m)))
nonlinear_model2 <- nls(H ~ Hm*(1-exp(-a*D/Hm)), data = data.frame(D = final_results$D_m, H = final_results$H_m), start = list(a = max(final_results$H_m), Hm = 35))
curve(Hm*(1-exp(-a*x/Hm)), add = TRUE, col = "black")

setwd("D:/ICL/Research Project/Data/Tree Ring Width/Ring Widths Simulation")
save (results, file = "Ring_Widths_Simulation3.RData")
save (final_results, file = "Ring_Widths_Simulation_df3.RData")


# Method 2
################################################################################

tmodel_dD <- function(
    D_m, P, L, a, c, Hm, ρs, rr, rs, ζ, y, σ, tf, tr, k) {
  
  # This code is taken directly from Guangqi Li's original R function
  
  # P0: potential annual GPP (P model). 10%: foliage matainance respiration
  GPP <- P * (1 - 0.1)
  # H is controlled by d, and constrained by maximum height (Hm)
  H <- Hm * (1 - exp(-a * D_m / Hm))
  # crown ratio
  fc <- H / (a * D_m)
  # crown area
  Ac <- ((pi * c) / (4 * a)) * D_m * H
  # stem mass
  Ws <- (pi / 8) * (D_m^2) * H * ρs
  # foliage mass
  Wf <- Ac * L * (σ^(-1))
  # sapwood mass
  Wss <- Ac * ρs * H * (1 - fc / 2) / c
  # GPP captured by crown
  P0 <- Ac * P
  # sapwood respiration
  Rm1 <- Wss * rs
  # fine root respiration
  Rm2 <- ζ * σ * Wf * rr
  # NPP after multiplied by the yield factor
  NPP1 <- y * (P0 - Rm1 - Rm2)
  # turnover of foliage and fine root
  NPP2 <- (Ac * L * ((1 / (σ * tf)) + (ζ / tr)))
  
  # Diameter increment dD
  num <- y * (P0 - ρs * (1 - H / (2 * a * D_m))
              * H * rs / c - L * ζ * rr
  ) - L * (1 / (σ * tf) + ζ * (1 / tr))
  den <- (a / (2 * c)) *
    ρs * (a * D_m * (1 / H - 1 / Hm) + 2) +
    (L / D_m) * (a * D_m * (1 / H - 1 / Hm) + 1) *
    (1 / σ + ζ)
  
  dD <- num / den
  
  # Stem increment dWs
  dWs <- (pi / 8 * ρs * D_m * (a * D_m * (1 - (H / Hm)) + 2 * H)) * dD
  # Foliage and fine root increment dwfr
  dWfr <- (L * ((pi * c) / (4 * a)) *
             (a * D_m * (1 - (H / Hm) + H)) * (1 / σ + ζ)
  ) * dD
  
  return(dD)
}

dD_uniroot_function <- function(
    D_m_t, D_m_t_plus1, GPP, L, a, c, Hm, ρs,
    rr, rs, ζ, y, σ, tf, tr, k) {
  # This function is designed to be used with uniroot - uniroot will
  # adjust the first argument (D_m_t) until it finds a value where the
  # return value is zero.
  
  # Returns (D_m_t + dD_t) - D_m_tplus1 for a given D_m
  
  dD <- tmodel_dD(
    D_m_t, GPP, L, a, c, Hm, ρs, rr, rs, ζ, y, σ, tf, tr, k
  )
  
  return((D_m_t + dD) - D_m_tplus1)
}

reverse_predict_D_m_dD <- function(
    D_m_T_plus1, GPP, L, a, c, Hm, ρs, rr,
    rs, ζ, y, σ, tf, tr, k) {
  # Normally, the tmodel takes D_m at time t and finds dD
  # for time t and hence D_m at time t+1:
  #
  #  D_m_t+1 = D_m_t + dD_t
  #
  # Here, we know D_m_t+1 and the conditions (GPP etc) for time t,
  # but not D_m_t. So we re-express this as:
  #
  #  (D_m_t + dD_t) - D_m_t+1  = 0
  #
  # We can then use a root solver (uniroot), to numerically find D_m_t
  # that satisfies the equation above.
  
  new_D_m <- uniroot(
    f = dD_uniroot_function, interval = c(0.0001, D_m_plus1), tol = 1e-9,
    D_m_t_plus1 = D_m_t_plus1, GPP = GPP, L = L, a = a, c = c,
    Hm = Hm, ρs = ρs, rr = rr, rs = rs, ζ = ζ, y = y,
    σ = σ, tf = tf, tr = tr, k = k
  )
  
  dD <- tmodel_dD(
    new_D_m$root, GPP, L, a, c, Hm, ρs, rr, rs, ζ, y, σ, tf, tr, K
  )
  
  return(c(new_D_m$root, dD))
}


# Data - dummy constant time series
trees <- Oak
D_m <- Oak$D_m
years <- sort (yearly_gpp_df$Year, decreasing = T)
gpp_time_series <- yearly_gpp_df$gpp
lai_time_series <- environment_df$LAI

# Set up the DBH and delta DBH stores

# initialize to store annual results
results <- list()
D_m <- numeric(nrow(trees))
dD <- numeric(nrow(trees))

for (year in years) {
  current_GPP <- P [yearly_gpp_df$Year == year]
  current_LAI <- L [environment_df$Year == year]
  for (i in nrow(trees)) {
  # get the previous year estimates of dbh and dD
  previous_year <- reverse_predict_D_m_dD(
    #D_m_tplus1 = D_m[i],
    GPP = P0[i],
    L = lai_time_series[i],
  )
  
  D_m[i + 1] <- previous_year[1]
  dD[i] <- previous_year[2]
  # store the result
  
  results[[as.character(year)]] <- data.frame(
    Year = year,
    ID = trees$ID,
    PlotID = trees$PlotID,
    GPP_kgC = P0,
    D_m = trees$D_m,
    dD = dD,

  )
  
  
  # renew D, H, As, Ac for the next loop
  
  trees$D_m <- new_D_m

  
  }

# Check the calculations - (dbh_t + dD_t) should equal dbh_t+1 to the
# precision limit of the uniroot finder.

(dbh[-1] + dD) - dbh[-(n_year + 1)]
}
