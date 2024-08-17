#continue detection
########################################################################
# 0.load the libraries
########################################################################
library(measuRing)
########################################################################
# 02.load the data
########################################################################
## set the working directory
setwd("D:/ICL/Research Project/Data/Tree Ring Width Statistics/Image section")
img_files<-list.files(path=getwd(), pattern = ".*.tif$", full.names=TRUE)

load(file='Silwood_tree_rings.RData')
load(file='Silwood_TR_incl.RData')
load(file='Silwood_TR_excl.RData')
########################################################################
########################################################################
# 03.to continue the detection
########################################################################
## 
############################################################
#loop the function from the last iteration
for (i in which(sapply(Silwood_tree_rings,is.null))[1]:length(img_files)){
	##detect
	Silwood_tree_rings[[i]]<-ringDetect (image = img_files[i],rgb = c(0.9,0,0.1), 
		segs = 1, origin = 0,last.yr =2023,darker = T,p.row = 0.75,ppi=600)
	
	###visual cleaning
	# RIGHT CLICK IF THERE IS NOTHING TO EXCLUDE OR INCLUDE
	#select points to exclude.
	Silwood_TR_excl[[i]]<-ringSelect(Silwood_tree_rings[[i]],any.col = FALSE) 
	#update detection
	Silwood_tree_rings[[i]] <- update(Silwood_tree_rings[[i]],exclu=Silwood_TR_excl[[i]])        
	#select points to include
	Silwood_TR_incl[[i]]<-ringSelect(Silwood_tree_rings[[i]],any.col = FALSE)     
	#update detection
	Silwood_tree_rings[[i]] <- update(Silwood_tree_rings[[i]],exclu=Silwood_TR_excl[[i]],inclu=Silwood_TR_incl[[i]])    
	##### rename dataframe
	
	names(Silwood_tree_rings[[i]]$ringWidths)<-c('year','width')
	#save
	save(Silwood_tree_rings,file='Silwood_tree_rings.RData')
	save(Silwood_TR_incl,file='Silwood_TR_incl.RData')
	save(Silwood_TR_excl,file='Silwood_TR_excl.RData')
	
}

