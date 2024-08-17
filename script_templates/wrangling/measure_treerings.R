#measure tree rings
########################################################################
# 0.load the libraries
########################################################################
library(measuRing)
########################################################################
# 02.load the data
########################################################################
## set the working directory
setwd("D:/ICL/Research Project/Data/Tree Ring Width Statistics/Image section")
##### read the name of the files
img_files<-list.files(path=getwd(), pattern = ".*.tif$", full.names = TRUE)

########################################################################
# 03. test detection algorithm, try different rgb and p.row 
########################################################################
##test for best parameter combination, rgb = c(0.9, 0, 0.1), p.row = 0.75

TR1 <- ringDetect (image = img_files[2], rgb = c(0.9,0,0.1), segs = 1, origin = 0, last.yr =2023, darker = T, p.row = 0.75, ppi=600)


# ####### do some manual cleaning
Toexc <- ringSelect(TR1, any.col = FALSE)                                  
TR2 <- update(TR1,exclu = Toexc)    

# ############################################################
# ###trick to plot again
TR2 <- update(TR2,last.yr =2023)

########################################################################
# 04. run for all the images
########################################################################
#create a list to save the results, name the elements as the images
Silwood_tree_rings<- vector(mode = "list", length = length(img_files))
names(Silwood_tree_rings)<-basename(img_files)
#create a list to save the rings to exclude
Silwood_TR_excl<-vector(mode = "list", length = length(img_files))
names(Silwood_TR_excl)<-basename(img_files)
#create a list to save the rings to include
Silwood_TR_incl<-vector(mode = "list", length = length(img_files))
names(Silwood_TR_incl)<-basename(img_files)


############################################################
#loop the function
for (i in 1:3){
	##detect
	Silwood_tree_rings[[i]]<-ringDetect (image = img_files[i],rgb = c(0.9,0,0.1), 
		segs = 1, origin = 0, last.yr =2023, darker = T, p.row = 0.75, ppi=600)
	
	###visual cleaning
	# RIGHT CLICK IF THERE IS NOTHING TO EXCLUDE OR INCLUDE
	#select points to exclude.
	cat('excluding points!')
	Silwood_TR_excl[[i]]<- ringSelect(Silwood_tree_rings[[i]],any.col = FALSE) 
	#update detection
	Silwood_tree_rings[[i]] <- update(Silwood_tree_rings[[i]],exclu=Silwood_TR_excl[[i]])        
	#select points to include
	cat('including points!')
	Silwood_TR_incl[[i]]<- ringSelect(Silwood_tree_rings[[i]],any.col = FALSE)     
	#update detection
	Silwood_tree_rings[[i]] <- update(Silwood_tree_rings[[i]],exclu=Silwood_TR_excl[[i]],inclu=Silwood_TR_incl[[i]])            
	##### rename dataframe
	
	names(Silwood_tree_rings[[i]]$ringWidths)<-c('year','width')
	#save
	save(Silwood_tree_rings,file='Silwood_tree_rings.RData')
	save(Silwood_TR_incl,file='Silwood_TR_incl.RData')
	save(Silwood_TR_excl,file='Silwood_TR_excl.RData')
	
}





