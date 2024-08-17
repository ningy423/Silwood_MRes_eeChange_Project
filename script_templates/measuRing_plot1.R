
install.packages('measuRing')
library(measuRing)
#Three other packages, pastecs, png, tiff are automatically loaded when measuRing is loaded

setwd("D:/ICL/Research Project/Data/Tree Ring Width/Image section")

img_files<-list.files(path=getwd(), pattern = ".*.tif$", full.names = TRUE)

###############################################################################
##Function ringDetect: detection of ring borders and plotting of image segments. 

## Plot 1 #####################################################################

## 1223_2180_1
plot1 <- ringDetect (image = img_files[5], rgb = c(0.9,0,0.1), origin = 0, last.yr =2023, darker = T, p.row = 1, ppi=600)
Width1 <- ringWidths(image = img_files[5], rgb = c(0.9,0,0.1), origin = 0, last.yr =2023, darker = T, p.row = 1, ppi=600)

## 1218_2175_1
plot2 <- ringDetect (image = img_files[4], darker = F, origin = 0, last.yr = 2023, ppi=600)
Width2 <- ringWidths (image = img_files[4], darker = F, origin = 0, last.yr = 2023, ppi=600)

## 1216_2173_1
plot3 <- ringDetect (image = img_files[3], origin = 0, last.yr = 2023, inclu = c(1690), ppi=600)
Width3 <- ringWidths (image = img_files[3], origin = 0, last.yr = 2023, inclu = c(1690), ppi=600)

## 1215_2172_1
plot4 <- ringDetect (image = img_files[2], rgb = c(0.9,0,0.1), origin = 0, p.row = 0.4, last.yr = 2023, exclu=c(2003), ppi=600)
Width4 <- ringWidths (image = img_files[2], rgb = c(0.9,0,0.1), origin = 0, p.row = 0.4, last.yr = 2023, exclu=c(2003), ppi=600)

## 1214_2171_1
plot5 <- ringDetect (image = img_files [1], rgb = c(0.9,0,0.1), origin = 0, p.row = 0.75, last.yr = 2023, ppi=600)
Width5 <- ringWidths (image = img_files [1], rgb = c(0.9,0,0.1), origin = 0, p.row = 0.75, last.yr = 2023, ppi=600)

## 1247_2214_1
plot6 <- ringDetect (image = img_files[12], rgb = c(0.9,0,0.1), origin = 0, p.row = 0.75, last.yr = 2023, ppi=600)
Width6 <- ringWidths (image = img_files[12], rgb = c(0.9,0,0.1), origin = 0, p.row = 0.75, last.yr = 2023, ppi=600)

## 1224_5867_1
plot7 <- ringDetect (image = img_files[6], origin = 0, last.yr = 2023, ppi=600)
Width7 <- ringWidths (image = img_files[6], origin = 0, last.yr = 2023, ppi=600)

## 1242_2204_1
plot8 <- ringDetect (image = img_files[7], darker = FALSE, origin = 0, last.yr = 2023, exclu=c(1381), ppi=600)
Width8 <- ringWidths (image = img_files[7], darker = FALSE, origin = 0, last.yr = 2023, exclu=c(1381), ppi=600)

## 1244_2207_1
plot9 <- ringDetect (image = img_files[9], rgb = c(0.9,0,0.1), darker = T, origin = 0, last.yr = 2023, exclu=c(1), ppi=600)
Width9 <- ringWidths (image = img_files[9], rgb = c(0.9,0,0.1), darker = T, origin = 0, last.yr = 2023, exclu=c(1), ppi=600)

## 1243_5873_1
plot10 <- ringDetect (image = img_files[8], darker = FALSE, origin = 0, p.row = 0.75, last.yr = 2023, inclu=c(0,1750), exclu= c(7, 1651, 1653), ppi=600)
Width10 <- ringWidths (image = img_files[8], darker = FALSE, origin = 0, p.row = 0.75, last.yr = 2023, inclu=c(0,1750), exclu= c(7, 1651, 1653), ppi=600)

## 4051_5872_1
plot11 <- ringDetect (image = img_files[27], origin = 0, last.yr = 2023, ppi=600)
Width11 <- ringWidths (image = img_files[27], origin = 0, last.yr = 2023, ppi=600)

## 4050_5871_1
plot12 <- ringDetect (image = img_files[25], origin = 0, last.yr = 2023, ppi=600)
Width12 <- ringWidths (image = img_files[25], origin = 0, last.yr = 2023, ppi=600)

## 4050_5871_2
plot13 <- ringDetect (image = img_files[26], origin = 0, last.yr = 2023, inclu = c(62), ppi=600)
Width13 <- ringWidths (image = img_files[26], origin = 0, last.yr = 2023, inclu = c(62), ppi=600)

## 1245_1440_1
plot14 <- ringDetect (image = img_files[10], darker = FALSE, origin = 0, last.yr = 2023, ppi=600)
Width14 <- ringWidths (image = img_files[10], darker = FALSE, origin = 0, last.yr = 2023, ppi=600)

## 1248_2215_1
plot15 <- ringDetect (image = img_files[13], darker = FALSE, origin = 0, last.yr = 2023, ppi=600)
Width15 <- ringWidths (image = img_files[13], darker = FALSE, origin = 0, last.yr = 2023, ppi=600)

## 4048_5868_1
plot16 <- ringDetect (image = img_files[23], darker = FALSE, origin = 0, last.yr = 2023, ppi=600)
Width16 <- ringWidths (image = img_files[23], darker = FALSE, origin = 0, last.yr = 2023, ppi=600)

## 4049_5869_1
plot17 <- ringDetect (image = img_files[24], origin = 0, p.row = 0.4, last.yr = 2023, ppi=600)
Width17 <- ringWidths (image = img_files[24], origin = 0, p.row = 0.4, last.yr = 2023, ppi=600)

## 1246_2212_1
plot18 <- ringDetect (image = img_files[11], origin = 0, p.row = 0.2, last.yr = 2023, exclu = c(21), ppi=600)
Width18 <- ringWidths (image = img_files[11], origin = 0, p.row = 0.2, last.yr = 2023, exclu = c(21), ppi=600)

## 1251_2218_1
plot19 <- ringDetect (image = img_files[14], origin = 0, p.row = 0.75, last.yr = 2023, exclu = c(1971), ppi=600)
Width19 <- ringWidths (image = img_files[14], origin = 0, p.row = 0.75, last.yr = 2023, exclu = c(1971), ppi=600)

## 4052_5874_1
plot20 <- ringDetect (image = img_files[28], origin = 0, last.yr = 2023, ppi=600)
Width20 <- ringWidths (image = img_files[28], origin = 0, last.yr = 2023, ppi=600)



## create a list to merge all data.frame (Width) ##############################

Ring_Widths_Plot1 <- list (Width1, Width2, Width3, Width4, Width5, Width6, Width7, Width8, Width9, Width10, 
                Width11, Width12, Width13, Width14, Width15, Width16, Width17, Width18, Width19, Width20)


## save data ##################################################################

save (Ring_Widths_Plot1, file = "Ring_Widths_Plot1.RData")

## do some plots ##########################################################

plot(Width1)
plot(Width8)


