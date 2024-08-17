#do some stats
########################################################################
# 0.load the libraries
########################################################################
library(measuRing)
########################################################################
# 02.load the data
########################################################################
## set th working directory
setwd("D:/ICL/Research Project/Data/Tree Ring Width Statistics/Image section")
##### read the name of the files
img_files<-list.files(path=getwd(), pattern = ".*.tif$", full.names=TRUE)

load(file='Silwood_tree_rings.RData')
load(file='Silwood_TR_incl.RData')
load(file='Silwood_TR_excl.RData')
########################################################################
# 04. do some plots
########################################################################

plot(Silwood_tree_rings[[1]]$ringWidths)
points(Silwood_tree_rings[[2]]$ringWidths,col=2)
points(Silwood_tree_rings[[3]]$ringWidths,col=3)
#############################################
###!!! IMPORTANT  the measuRing package saves the code used, plus the manual detections. So, it needs the iterator 'i' to recreate individual plots again
##plot tree rings
i=2
eval(Silwood_tree_rings[[i]]$call)



