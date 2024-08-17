#optimize Tmod OG a-c-z
library(dfoptim)
##wrap function
wrap_tmodel_opt<-function(param){
	###define parameters
	a_hd <- param[1]  # initial slope of height-diameter relationship
	ca_ratio <- param[2]# initial ratio of crown area to stem cross-sectional area
	#Hm <-   # asymptotic maximum Oak height
	#rho_s <- 538 param[2]   # sapwood density in kg C m-3
	#zeta <- param[3]   # zeta, Ratio of fine-root mass to foliage area (kgCm-2)
	#tau_f <- 1.0 # tau_f, Foliage turnover time (years)
	#tau_r <- 1.27 param[2] # tau_r, Fine-root turnover time (years)
	zeta <- param[3]   # zeta, Ratio of fine-root mass to foliage area (kgCm-2)
	#yld <- 0.6 # y, Yield_factor (-)
	#resp_r <- 0.913 param[3] # r_r, Fine-root specific respiration rate (year-1)
	#resp_s <- 0.0419 # r_s, Sapwood-specific respiration rate (year-1)
	#################################################################################################
	#piDH_4a <- (pi*Oak$D_m*Oak$H_m)/(4*a_hd)
	###Eq8 Li et al.,(2014, BG) 
	#lm_model <- summary (lm(Ac_m2 ~ piDH_4a+0,data=Oak))
	#ca_ratio <- lm_model$coefficients["piDH_4a","Estimate"]  # initial ratio of crown area to stem cross-sectional area
	max_LAI_ind=1/(ca_ratio*Hv)
	#################################################################################################
	Tmodel_wrap=function(df,dbh_last_obs,a_hd,ca_ratio,zeta){
		# ###CALC C
		# piDH_4a <- (pi*Oak$D_m*Oak$H_m)/(4*a_hd)
		# ###Eq8 Li et al.,(2014, BG) 
		# lm_model <- summary (lm(Oak$Ac ~ piDH_4a+0))
		# ca_ratio <- lm_model$coefficients["piDH_4a","Estimate"]  # initial ratio of crown area to stem cross-sectional area
		
		# Data - dummy constant time series
		
		n_year <- sort (df$Year, decreasing = T)
		###potential gpp
		gpp_time_series <- df$P0
		#LAI per tree
		lai_time_series <- (1-((max_LAI_eco-df$LAI)/max_LAI_eco))*max_LAI_ind
		
		# Initialize the DBH and delta DBH series
		dbh <- numeric(length(n_year));dbh[1]=dbh_last_obs
		delta_dbh <- numeric(length(n_year))
		###run T model in reverse
		for (idx in 1:length(n_year)) {
			# get the previous year estimates of dbh and delta_dbh
			# previous_year <- reverse_predict_dbh_delta_dbh(
			# 	dbh_tplus1 = dbh[idx],
			# 	GPP = gpp_time_series[idx],
			# 	L = lai_time_series[idx],
			# 	a = a_hd, cr = ca_ratio, Hm = h_max, rho = rho_s, rr = resp_r,
			# 	rs = resp_s, zeta = zeta, y = yld, sigma = sla, tf = tau_f,
			# 	tr = tau_r, K = par_ext
			# )
			# dbh[idx + 1] <- previous_year[1]
			# delta_dbh[idx] <- previous_year[2]
			try(previous_year <- reverse_predict_dbh_delta_dbh(
				dbh_tplus1 = dbh[idx],
				GPP = gpp_time_series[idx],
				L = lai_time_series[idx],
				a = a_hd, cr = ca_ratio, Hm = h_max, rho = rho_s, rr = resp_r,
				rs = resp_s, zeta = zeta, y = yld, sigma = sla, tf = tau_f,
				tr = tau_r, K = par_ext
				), silent=T)
			# if(class(previous_year)=="try-error"){
			# 	dbh[idx + 1] <- dbh[idx]
			# 	delta_dbh[idx] <- 0
			# }else{
			# dbh[idx + 1] <- previous_year[1]
			# delta_dbh[idx] <- previous_year[2]
			# }
			dbh[idx + 1] <- previous_year[1]
			delta_dbh[idx] <- previous_year[2]
		}
		df$rw_sim_mm=(delta_dbh/2)*1000
		return(df)
	}
	#y <- param[1]    # yield factor 
	#r_l_ratio <-  param[2]   # the ratio of fine-root mass to foliage area in kg C m-2
	#rr <-  param[3]  # specific respiration rate of fine-root in mol C mol-1 year-1
	#rs <-  param[4]  # specific respiration rate of sapwood in mol C mol-1 year-1
	Ring_Widths_Observation<-mapply(Tmodel_wrap,Ring_Widths_Observation,dbh_last_obs=Oak$D_m,MoreArgs = list(a_hd,ca_ratio,zeta),SIMPLIFY = F)
	fulldb <<- do.call(rbind,Ring_Widths_Observation)

	msep_sim=MSEP_comp(fulldb$rw_sim_mm,fulldb$Width)
	msep_sim['MSEP']
	#-hydroGOF::KGE(fulldb$rw_sim_mm,fulldb$Width)
	
}


################################################################################
################################################################################
###LOWER END
################################################################################
# a_hd_opt <- 35  # initial slope of height-diameter relationship
# c_opt <- 220 # tau_r, Fine-root turnover time (years)
# zeta_opt <- 0.17 # zeta, Ratio of fine-root mass to foliage area (kgCm-2)
# opt_par=c(a_hd_opt,c_opt,zeta_opt)
# lower = c(34,215,0.16)
# upper=  c(37,225,0.18)
# 
# Tmodel_par_optim<-dfoptim::nmkb(opt_par,
# 	fn=wrap_tmodel_opt,
# 	lower=lower,
# 	upper=upper
# )

################################################################################
################################################################################
###HIGHER END
################################################################################
a_hd_opt <- 300  # initial slope of height-diameter relationship
c_opt <- 400 # ratio of crown area to stem cross-sectional area
zeta_opt <- 0.17 # zeta, Ratio of fine-root mass to foliage area (kgCm-2)
opt_par=c(a_hd_opt,c_opt,zeta_opt)
lower = c(90,350,0.1)
upper=  c(400,590,0.2)

Tmodel_par_optim<-dfoptim::nmkb(opt_par,
	fn=wrap_tmodel_opt,
	lower=lower,
	upper=upper
)

str(fulldb)

plot(fulldb$Year,fulldb$rw_sim_mm,col=as.factor(fulldb$ID),xlab="Year",ylab = "Ring Widths (mm)")
plot(fulldb$rw_sim_mm~as.factor(fulldb$Year),xlab="Year",ylab = "Ring Widths (mm)")

colnames(fulldb) <- c("Year","rw_obs_mm","Age","ID","Plot","cum_rw","tc_max","tc_min","tc","pn","RH","sw_in","fapar","co2","soilm","vpd","meanalpha",
                   "ppfd","LAI","yearly_GPP","P0","rw_sim_mm")

setwd("D:/ICL/Research Project/Data/Merge")
save(fulldb, file = "Merge2.RData")
