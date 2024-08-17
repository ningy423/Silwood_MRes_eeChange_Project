MSEP_comp<-function(sim,obs,...){
	# ************************************************************************
	# Name:     MSEP_comp
	# Inputs:   - double - vector (sim), simulations
	#           - double - vector (obs), observations
	# Returns:  double, vector MSEP,MC,SC,RC
	# Features: This function calculates the mean squared error of prediction and ots components:bias error (MC), slope error (SC) and random error (RC)
	# * Ref:    Haefner, 2005. Modeling Biological Systems: Principles and Applications. Springer
	# ************************************************************************
	###############################################################################################
	# 01.define vars
	###############################################################################################
	X=mean(sim,na.rm=T);Y=mean(obs,na.rm=T);Sx=sd(sim,na.rm=T);Sy=sd(obs,na.rm=T);r=cor(sim,obs,use="complete.obs")
	#bias error
	MCe=(X-Y)^2
	##slope error
	SCe=(Sx-r*Sy)^2
	##random error
	RCe=(1-r^2)*Sy^2
	###mean squared error of prediction
	MSEP=MCe+SCe+RCe
	err=c(MSEP=MSEP,MC=MCe/MSEP,SC=SCe/MSEP,RC=RCe/MSEP)
	return(err)
	
}
