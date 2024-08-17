#stats
########################################################################
# 0.load the libraries
########################################################################
#library(measuRing)
########################################################################
# 02.load the data
########################################################################
## set the working directory
setwd("D:/ICL/Research Project/Data/Tree Ring Width Statistics/Silwood_data_to_curate")
##### read the name of the files
load(file ='Ring_Widths_Plot.RData')
########################################################################

# 03. add age and ID
########################################################################
add_age_ID <- function(df){
	#get the age
	df$age=seq(length(df$year),1,-1)
	#get ID
	df$ID<-rep(names(df)[2],length(df$year))
	##rename the width column
	names(df)[2]<-'delta_width'
	##get Plot
	df
	#result
	return(df)
}
#####################################################
lapplyRing_Widths_Plot<-lapply(Ring_Widths_Plot,add_age_ID)
####################################################
#merge in one df
Ring_Widths_df<-do.call(rbind,lapplyRing_Widths_Plot)
#########################################################################################

#do some plots 
hist(Ring_Widths_df$delta_width)

#plot of Widths ~ Age
plot(Ring_Widths_df$age,Ring_Widths_df$delta_width,col=as.factor(Ring_Widths_df$ID),pch=16)
plot(Ring_Widths_df$delta_width~as.factor(Ring_Widths_df$age), xlab="Age", ylab = "Width (mm)",col=3)

#plot of Widths ~ Year
plot(Ring_Widths_df$year,Ring_Widths_df$delta_width,col=as.factor(Ring_Widths_df$ID),pch=16)
plot(Ring_Widths_df$delta_width~as.factor(Ring_Widths_df$year), xlab="Year", ylab = "Width (mm)",col=3)


library(ggplot2)
ggplot (Ring_Widths_df, aes(x=age, y=delta_width, col=as.factor(Ring_Widths_df$ID))) + geom_point() + labs(x="Age",y="Width (mm)")
ggplot (Ring_Widths_df, aes(x=age, y=delta_width, col=as.factor(Ring_Widths_df$ID))) + geom_line() + labs(x="Age",y="Width (mm)") 
