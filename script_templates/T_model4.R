
setwd("D:/ICL/Research Project/Data/Core Samples")
Oak <- read.csv(file = "Oak_Data_EC.csv") 


### Functional Geometric Relationship
###############################################################################

## regress Hm and a, with equation H = Hm*(1-exp(-a*D/Hm))

plot (Oak$D_m, Oak$H_m, xlab = "Diameter at Breast Height (m)", ylab = "Height (m)", xlim = c(0,max(Oak$D_m)), ylim = c(0,max(Oak$H_m)))

nonlinear_model <- nls(H ~ Hm*(1-exp(-a*D/Hm)), data = data.frame(D = Oak$D_m, H = Oak$H_m), start = list(a = 50, Hm = max(Oak$H_m)))

a <- coef(nonlinear_model)[["a"]]  # initial slope of height-diameter relationship

Hm <- coef(nonlinear_model)[["Hm"]]  # asymptotic maximum Oak height

curve(Hm*(1-exp(-a*x/Hm)), add = TRUE, col = "#4169E1", lwd=2, lty=2)

a <- 399.9995

curve(Hm*(1-exp(-a*x/Hm)), add = TRUE, col = "blue", lwd=2)



## regress c, with linear model between Ac and piDH/4a

piDH_4a <- (pi*Oak$D_m*Oak$H_m)/(4*a)

plot (piDH_4a, Oak$Ac_m2, xlab = "piDH/4a (m2)", ylab = "Crown Area (m2)", xlim = c(0, max(piDH_4a)), ylim = c(0, max(Oak$Ac)))

lm_model <- lm(Oak$Ac_m2 ~ piDH_4a+0)

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

ρs <- 538   # sapwood density in kg C m-3
k <- 0.5   # PAR extinction coefficient
y <- 0.6    # yield factor
ζ <- 0.17    # the ratio of fine-root mass to foliage area in kg C m-2
σ <- 25.08    # specific leaf area in m2 kg-1 
rr <- 0.913   # specific respiration rate of fine-root in mol C mol-1 year-1
rs <- 0.0419   # specific respiration rate of sapwood in mol C mol-1 year-1
τf <- 1.0   # foliage turnover time in years
τr <- 1.27   # fine-root turnover time in years
Hv <- 0.000398 # Quercus ilex Flo et al., (2021, NPh)
environment_df$P0 <- yearly_gpp_df$gpp/environment_df$fapar  # actual gpp in kg C m-2
environment_df$LAI <- (1-((max(environment_df$LAI)-environment_df$LAI)/max(environment_df$LAI)))/(c*Hv)   # leaf area index in m2 m-2

ρs <- 574.7   # sapwood density in kg C m-3
y <- 0.6    # yield factor
ζ <- 0.1000009    # the ratio of fine-root mass to foliage area in kg C m-2
σ <- 25.08    # specific leaf area in m2 kg-1 
rr <- 1.23  # specific respiration rate of fine-root in mol C mol-1 year-1
rs <- 0.034   # specific respiration rate of sapwood in mol C mol-1 year-1
τf <- 1.0   # foliage turnover time in years
τr <- 1.27   # fine-root turnover time in years

# abstract years and order by decreasing
years <- sort (yearly_gpp_df$Year, decreasing = T)
trees <- Oak
results <- list()

# define objective function
objective_function <- function(x, current_D, current_Ac, previous_D, previous_H, previous_Ac, previous_fc, Hm, a, y, tree, current_GPP, current_LAI, c, rs, rr, ζ, σ, τf, τr, ρs) {
  # x as previous year's diameter
  previous_D <- x
  previous_H <- Hm * (1 - exp(-a * previous_D / Hm))
  dH_dD <- a * exp(-a * previous_D / Hm)
  dD_dt <- current_D - previous_D
  dH_dt <- dD_dt*dH_dD
  dAc_dt <- (pi*c/(4*a))*(dD_dt*previous_H + dH_dt*previous_D)
  previous_Ac <- current_Ac - dAc_dt
  previous_fc <- previous_H/(a*previous_D)
  A <- y * previous_Ac * (current_GPP - ρs * (1 - previous_fc / 2) * previous_H * rs / c - current_LAI * ζ * rr) - current_LAI * previous_Ac * (1 / (σ * τf) + ζ / τr)
  B <- current_LAI * (pi * c / (4 * a)) * (a * previous_D * (1 - previous_H / Hm) + previous_H) * (1 / σ + ζ) + (pi / 8) * ρs * (previous_D^2 * dH_dD + 2 * previous_D * previous_H)
  return(abs(A / B + dD_dt))
}

# 逐棵树计算
for (i in 1:nrow(trees)) {
  tree <- trees[i, ]
  
  # 初始化
  current_D <- tree$D_m
  current_H <- tree$H_m
  current_Ac <- tree$Ac
  current_fc <- tree$fc
  annual_results <- data.frame()
  
  for (year in years) {
    # 当前年份的GPP和LAI
    current_GPP <- environment_df$P0[environment_df$Year == year]
    current_LAI <- environment_df$LAI[environment_df$Year == year]
    
    # 计算GPP
    tree$gpp <- current_GPP * current_Ac
    
    # 使用优化方法找到前一年的胸径
    previous_D <- optim(par = current_D, 
                        fn = objective_function, 
                        current_D = current_D, 
                        current_Ac = current_Ac, 
                        current_GPP = current_GPP,
                        current_LAI = current_LAI,
                        Hm = Hm, 
                        a = a, 
                        y = y, 
                        tree = tree, 
                        c = c, 
                        rs = rs, 
                        rr = rr, 
                        ζ = ζ, 
                        σ = σ, 
                        τf = τf, 
                        τr = τr, 
                        ρs = ρs)$par
    
    previous_H <- Hm * (1 - exp(-a * previous_D / Hm))
    
    dH_dD <- a * exp(-a * previous_D / Hm)
    
    dD_dt <- current_D - previous_D
    
    dWs_dt <- (pi / 8) * ρs * (previous_D^2 * dH_dD + 2 * previous_D * previous_H) * dD_dt
    
    dH_dt <- dD_dt * dH_dD

    dAc_dt <- (pi*c/(4*a)) * (previous_H * dD_dt + previous_D * dH_dt)
    
    previous_fc <- previous_H/(a*previous_D)

    annual_ring_width <- dD_dt / 2*1000

    if (annual_ring_width < 0) {
      annual_ring_width <- NA
    }
    
    # 更新树的参数
    current_D <- previous_D
    current_H <- previous_H
    current_Ac <- current_Ac - dAc_dt
    current_fc <- previous_fc
    
    # 保存结果
    annual_results <- rbind(annual_results, data.frame(
      ID = tree$ID,
      Year = year,
      D_m = current_D,
      H_m = current_H,
      Ac = current_Ac,
      tree_GPP = tree$gpp,
      dD_dt = dD_dt,
      dH_dt = dH_dt,
      dWs_dt = dWs_dt,
      rw_simulation_mm = annual_ring_width
    ))
  }
  
  # 将每棵树的年度结果保存到总结果中
  results[[as.character(tree$ID)]] <- annual_results
}

# 将所有树木的数据合并到一个数据框
final_results <- do.call(rbind, results)














###############################################################################
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
################################################################################
# ring_widths_simulation_31 is the result with σ=14.72, 
# ring_widths_simulation_32 is the result with σ=25.08 and Ac iteration dAc_dt <- (pi*c/(4*a))*dD_dt*dH_dt
# ring_widths_simulation_33 is the result with σ=25.08, Ac iteration and fc iteration of new_fc <- new_H_m/(a*new_D_m)  
# ring_widths_simulation_34 is the result with assuming the previous_D + dD_dt = current_D, and Ac and fc iteration
# ring_widths_simulation_35 is the result with tree-by-tree calculation assuming previous_D + dD_dt = current_D  ### the most accurate one so far

plot(final_results$Year,final_results$rw_simulation_mm,col=as.factor(final_results$ID), xlab = "Year", ylab = "Ring Widths (mm)")
plot(final_results$rw_simulation_mm ~ as.factor(final_results$Year), xlab = "Year", ylab = "Ring Widths (mm)")
filtered_data <- subset(final_results, ID == "1223_2180_1111")
plot(filtered_data$Year, filtered_data$ring_width_mm, type = "o", col = "blue")

plot(final_results$D_m,final_results$H_m, col=as.factor(final_results$ID), xlim = c(0,max(final_results$D_m)), ylim = c(0,max(final_results$H_m)))
nonlinear_model2 <- nls(H ~ Hm*(1-exp(-a*D/Hm)), data = data.frame(D = final_results$D_m, H = final_results$H_m), start = list(a = max(final_results$H_m), Hm = 35))
curve(Hm*(1-exp(-a*x/Hm)), add = TRUE, col = "black")

setwd("D:/ICL/Research Project/Data/Tree Ring Width/Ring Widths Simulation")
save (results, file = "Ring_Widths_Simulation5.RData")
save (final_results, file = "Ring_Widths_Simulation_df5.RData")

