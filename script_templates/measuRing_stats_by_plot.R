#stats
########################################################################
# 0.load the libraries
########################################################################
#library(measuRing)
########################################################################
# 02.load the data
########################################################################
## set the working directory
setwd("D:/ICL/Research Project/Data/Tree Ring Width/Ring Widths Observation")
##### read the name of the files
load(file ='Ring_Widths_Plot1.RData')
load(file ='Ring_Widths_Plot2.RData')
load(file ='Ring_Widths_Plot3.RData')
########################################################################

# 03. add age, ID, and Plot No.
########################################################################
add_age_ID_Plot1 <- function(df){
	#get the age
	df$age=seq(length(df$year),1,-1)
	#get ID
	df$ID<-rep(names(df)[2],length(df$year))
	##rename the width column
	names(df)[2]<-'delta_width'
	##get Plot
	df$Plot<-1
	#result
	return(df)
}

add_age_ID_Plot2 <- function(df){
  #get the age
  df$age=seq(length(df$year),1,-1)
  #get ID
  df$ID<-rep(names(df)[2],length(df$year))
  ##rename the width column
  names(df)[2]<-'delta_width'
  ##get Plot
  df$Plot<-2
  #result
  return(df)
}

add_age_ID_Plot3 <- function(df){
  #get the age
  df$age=seq(length(df$year),1,-1)
  #get ID
  df$ID<-rep(names(df)[2],length(df$year))
  ##rename the width column
  names(df)[2]<-'delta_width'
  ##get Plot
  df$Plot<-3
  #result
  return(df)
}
#####################################################
Ring_Widths_Plot1<-lapply(Ring_Widths_Plot1,add_age_ID_Plot1)
Ring_Widths_Plot2<-lapply(Ring_Widths_Plot2,add_age_ID_Plot2)
Ring_Widths_Plot3<-lapply(Ring_Widths_Plot3,add_age_ID_Plot3)
####################################################

# 04. merge in one df
Ring_Widths_df1<-do.call(rbind,Ring_Widths_Plot1)
Ring_Widths_df2<-do.call(rbind,Ring_Widths_Plot2)
Ring_Widths_df3<-do.call(rbind,Ring_Widths_Plot3)
Ring_Widths_df<-rbind(Ring_Widths_df1,Ring_Widths_df2,Ring_Widths_df3)
#############################################################################

# 05. save the result
save (Ring_Widths_df1, file = "Ring_Widths_df1.RData")
save (Ring_Widths_df2, file = "Ring_Widths_df2.RData")
save (Ring_Widths_df3, file = "Ring_Widths_df3.RData")
save (Ring_Widths_df, file = "Ring_Widths_df.RData")
write.csv(Ring_Widths_df, "Ring_Widths_df.csv")

# 05. do some plots 
#############################################################################
hist(Ring_Widths_df$delta_width)

#plot of Width ~ Age grouped by plot
library(dplyr)
library(ggplot2)

# Scatterplot by plot number
plot (Ring_Widths_df$age, Ring_Widths_df$delta_width, col = as.factor(Ring_Widths_df$Plot), pch=18, xlab = "Age", ylab = "Width (mm)")
ggplot (Ring_Widths_df, aes(x=age, y=delta_width, col=as.factor(Ring_Widths_df$Plot))) + geom_point() + labs(x="Age",y="Width (mm)") + facet_grid(.~Plot)

# Boxplot by plot number
par(mfrow=c(2,2))
plot(Ring_Widths_df$delta_width~as.factor(Ring_Widths_df$age), ylim=c(0,15), xlab="Age", ylab = "Width (mm)",col=3)
plot(Ring_Widths_df1$delta_width~as.factor(Ring_Widths_df1$age), ylim=c(0,15),xlab="Age", ylab = "Width (mm)", main="Plot1",col=4)
plot(Ring_Widths_df2$delta_width~as.factor(Ring_Widths_df2$age), ylim=c(0,15), xlab="Age", ylab = "Width (mm)",main="Plot2",col=5)
plot(Ring_Widths_df3$delta_width~as.factor(Ring_Widths_df3$age), ylim=c(0,15), xlab="Age", ylab = "Width (mm)",main="Plot3",col=6)

# Boxplot by age group ############################################################
breaks <- c(-Inf, 15, 30, Inf)
labels <- c("0-15", "15-30", ">30")
Ring_Widths_df$Age_Group <- cut(Ring_Widths_df$age, breaks = breaks, labels = labels, include.lowest = TRUE)
ggplot(Ring_Widths_df, aes(x = Age_Group, y = delta_width)) +
  geom_violin() + coord_flip()

# Line chart by plot number
Mean_Width_by_age <- Ring_Widths_df %>% group_by(age) %>% summarise (Mean=mean(delta_width,na.rm = T), SD = sd(delta_width ))
ggplot (Mean_Width_by_age, aes(x=age, y= Mean, col=2)) + geom_line() + geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD)) +
       labs(x="Age",y="Width (mm)")

Mean_Width_by_age_plot1 <- Ring_Widths_df1 %>% group_by(age) %>% summarise(Mean=mean(delta_width), SD=sd(delta_width))
Mean_Width_by_age_plot2 <- Ring_Widths_df2 %>% group_by(age) %>% summarise(Mean=mean(delta_width), SD=sd(delta_width))
Mean_Width_by_age_plot3 <- Ring_Widths_df3 %>% group_by(age) %>% summarise(Mean=mean(delta_width), SD=sd(delta_width))


ggplot (data=Mean_Width_by_age_plot1, aes(x=age, y= Mean)) + geom_line(color=1,size=1.2) + geom_point(size=2) +
       geom_line(data=Mean_Width_by_age_plot2, aes(x=age, y= Mean), color="#1E90FF",size=1.2) + geom_point(data = Mean_Width_by_age_plot2, col="#1E90FF",size=2) +
       geom_line(data=Mean_Width_by_age_plot3, aes(x=age, y= Mean), color="#32CD32",size=1.2) + geom_point(data = Mean_Width_by_age_plot3,col="#32CD32",size=2) +
       geom_errorbar(data = Mean_Width_by_age_plot3, aes(ymin=Mean-SD, ymax=Mean+SD),col="#32CD32") + 
       geom_errorbar(data = Mean_Width_by_age_plot2, aes(ymin=Mean-SD, ymax=Mean+SD),col="#1E90FF") +
       geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD),col="#808080",size=0.75) +
       theme_minimal()
 