##Tmodel OG
###################################################################################################
# 01. load the data
###################################################################################################
##load tree allometry
setwd("D:/ICL/Research Project/Data/Core Samples")
Oak <- read.csv(file = "Oak_Data_EC.csv") 
#load tree rings measurements
Ring_Widths_Observation_df <- read.csv("../Tree Ring Width/Ring Widths Observation/Ring_Widths_df.csv")
Ring_Widths_Observation<-split(Ring_Widths_Observation_df,Ring_Widths_Observation_df$ID)
##organize time series
Ring_Widths_Observation <- lapply(Ring_Widths_Observation, function(df){df[order(df$Age),]})
#get cumulative ring width
Ring_Widths_Observation <- lapply(Ring_Widths_Observation,function(df){df$cum_rw=cumsum(df$Width);df})
##extract final cumulative ring width
final_sapwood_width<-lapply(Ring_Widths_Observation,function(df){subset(df,df$Year==2023)})
final_sapwood_width<-do.call(rbind,final_sapwood_width)
#full_ID_widths<-do.call(rbind,strsplit(as.character(final_sapwood_width$ID),'_'))
#final_sapwood_width$full_ID<-paste0(full_ID_widths[,1],'_',full_ID_widths[,2],'_',full_ID_widths[,3])
#merge with dbh and H
#Oak$full_ID<-paste0(Oak$TreeID,'_',Oak$MarkNumber,'_',Oak$Stem,Oak$Stem,Oak$Stem,Oak$Stem)
Oak<-merge(Oak,final_sapwood_width,by=c('ID','Year'))
D_rw_lm<-lm(D_m~cum_rw,data=Oak)
##load anvironmental data and gpp
setwd("../Merge")
load("yearly_gpp_df.RData")
load("environment_df.RData")
##merge biophysical data
Silwood_biophysical=merge(environment_df,yearly_gpp_df,by='Year')
#calculate potential gpp
Silwood_biophysical$P0 = Silwood_biophysical$yearly_GPP_kg_m2/Silwood_biophysical$fapar 
###Merge biophysical with tree data
Ring_Widths_Observation<-mapply(function(x,y){merge(x,y,by='Year')},x=Ring_Widths_Observation,MoreArgs = list(y=Silwood_biophysical),SIMPLIFY = F)


#  02. load the code
###################################################################################################
source('../script_templates/Wrangling/t_model_reverse.R')
##write wrapper t model
Tmodel_wrap=function(df,dbh_last_obs){
  
	# Data - dummy constant time series
	n_year <- sort (df$Year, decreasing = T)
	###potential gpp
	gpp_time_series <- df$P0
	#LAI per tree
	lai_time_series <- (1-((max_LAI_eco-df$LAI)/max_LAI_eco))*max_LAI_ind
	
	# initialize DBH and delta DBH series
	dbh <- numeric(length(n_year));dbh[1]=dbh_last_obs
	delta_dbh <- numeric(length(n_year))
	
	###run T model in reverse
	for (idx in 1:length(n_year)) {
		# get the previous year estimates of dbh and delta_dbh
		try(previous_year <- reverse_predict_dbh_delta_dbh(
			dbh_tplus1 = dbh[idx],
			GPP = gpp_time_series[idx],
			L = lai_time_series[idx],
			a = a_hd, cr = ca_ratio, Hm = h_max, rho = rho_s, rr = resp_r,
			rs = resp_s, zeta = zeta, y = yld, sigma = sla, tf = tau_f,
			tr = tau_r, K = par_ext
		), silent=T)
		
		dbh[idx + 1] <- previous_year[1]
		delta_dbh[idx] <- previous_year[2]
	}
	df$rw_sim_mm=(delta_dbh/2)*1000
	return(df)
}


#  03. get allometric coefficients
###################################################################################################
## regress Hm and a, with nonlinear model for H and D
par(mfrow=c(1,2))
D <- Oak$D_m
H <- Oak$H_m
plot (Oak$D_m, Oak$H_m, xlab = "Diameter at Breast Height (m)", ylab = "Height (m)", xlim = c(0,0.6), ylim = c(0,34), pch=16)
nonlinear_model <- nls(H ~ Hm*(1-exp(-a*D/Hm)), data = data.frame(D = D, H = H), start = list(a = 50, Hm = 30))
Hm <- coef(nonlinear_model)[["Hm"]]  # asymptotic maximum Oak height
a <- coef(nonlinear_model)[["a"]]  # initial slope of height-diameter relationship
curve(Hm*(1-exp(-a*x/Hm)), add = TRUE, col = "#FF8C00", lwd=2)
nonlinear_p <- summary(nonlinear_model)$coefficients[1, 4]  
text(min(D)+0.25, min(H)+4, labels = paste0("H = Hm(1-exp(-aD/Hm))"))
text(min(D)+0.3, min(H)+2, labels = paste0("P = ", round(nonlinear_p, 4)), pos = 4)
text(min(D)+0.2, min(H), labels = paste0("Hm = ", round(Hm, 4)), pos = 4)
text(min(D)+0.21, min(H)-2, labels = paste0("a = ", round(a, 4)), pos = 4, col = "#FF8C00")
a <- 399.9995
curve(Hm*(1-exp(-a*x/Hm)), add = TRUE, col = "blue", lwd=2, lty=2)
text(min(D)+0.09, min(H)-4, labels = paste0("Optimized a =", round(a, 4)), pos = 4,col="blue")

## regress c, with linear model for Ac and piDH/4a
piDH_4a <- (pi*Oak$D_m*Oak$H_m)/(4*a)
Ac <- Oak$Ac_m2
###Eq8 Li et al.,(2014, BG) 
plot (piDH_4a, Ac, xlab = "πDH/4a (m2)", ylab = "Crown Area (m2)", xlim = c(0, max(piDH_4a)), ylim = c(0, max(Oak$Ac)),pch=16)
lm_model <- lm(Ac ~ piDH_4a+0)
summary_lm <- summary(lm_model)
c <- summary_lm$coefficients["piDH_4a","Estimate"]  # initial ratio of crown area to stem cross-sectional area
abline (lm_model, col="#FF8C00", lwd=2)
linear_p <- summary_lm$coefficients[4]
text(min(piDH_4a)+0.004, max(Ac), labels = paste0("Ac = πcDH/4a"))
text(min(piDH_4a), max(Ac)-10, labels = paste0("P = ", round(linear_p, 4)), pos = 4)
text(min(piDH_4a), max(Ac)-20, labels = paste0("c = ", round(c, 4)), pos = 4, col="#FF8C00")
c <- 485.2062
abline (0, c, col="blue", lwd=2, lty=2)
text(min(piDH_4a), max(Ac)-30, labels = paste0("Optimized c = ", round(c, 4)), pos = 4, col = "blue")

#  04. Set the parameters
###################################################################################################
Hv <- 0.000398 # Quercus ilex Flo et al., (2021, NPh)
max_LAI_eco =max(Silwood_biophysical$LAI)
max_LAI_ind=1/(c*Hv)
a_hd <- a
ca_ratio <- c
h_max <- Hm # H_m, Maximum tree height (m)
rho_s <- 574.7 # rho_s, Sapwood density (kgCm-3)
sla <- 12.94 # sigma, Specific leaf area (m2 kg-1C)
tau_f <- 1.0 # tau_f, Foliage turnover time (years)
tau_r <- 0.79 # tau_r, Fine-root turnover time (years)
#tau_r=Tmodel_par_optim$par[2]
par_ext <- 0.5 # k, PAR extinction coefficient (-)
yld <- 0.6 # y, Yield_factor (-)
zeta <- 0.17 # zeta, Ratio of fine-root mass to foliage area (kgCm-2)
#zeta <- 0.1000
#zeta=Tmodel_par_optim$par[3]
resp_r <- 1.23 # r_r, Fine-root specific respiration rate (year-1)
resp_s <- 0.034 # r_s, Sapwood-specific respiration rate (year-1)
resp_f <- 0.1 # --- , Foliage maintenance respiration fraction (-)

###################################################################################################
#  05. Run Tmodel per tree
###################################################################################################
Ring_Widths_Observation<-mapply(Tmodel_wrap,Ring_Widths_Observation,dbh_last_obs=Oak$D_m,SIMPLIFY = F)
fulldb=do.call(rbind,Ring_Widths_Observation)
colnames(fulldb) <- c("Year","rw_obs","Age","ID","Plot","cum_rw_mm","tc_max","tc_min","tc","pn","RH","sw_in",
                      "fapar","co2","soilm","vpd","meanalpha","ppfd","LAI","yearly_GPP","P0","rw_sim1")

a <- 399.9995 #calibrated a
c <- 485.2062 #calibrated c
a_hd <- a
ca_ratio <- c
zeta <- 0.1000009

Ring_Widths_Observation<-mapply(Tmodel_wrap,Ring_Widths_Observation,dbh_last_obs=Oak$D_m,SIMPLIFY = F)
fulldb2=do.call(rbind,Ring_Widths_Observation)
colnames(fulldb2) <- c("Year","rw_obs","Age","ID","Plot","cum_rw_mm","tc_max","tc_min","tc","pn","RH","sw_in",
                      "fapar","co2","soilm","vpd","meanalpha","ppfd","LAI","yearly_GPP","P0","rw_sim2")

graphics.off()
plot(fulldb2$Year,fulldb2$rw_sim2,col=as.factor(fulldb2$ID),xlab="Year",ylab = "Ring Widths (mm)")

fulldb$rw_sim2 <- fulldb2$rw_sim2 

setwd("D:/ICL/Research Project/Data/Merge")
save(fulldb,file = "Merge1.RData")

