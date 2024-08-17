
install.packages('measuRing')
library(measuRing)
#Three other packages, pastecs, png, tiff are automatically loaded when measuRing is loaded

setwd("D:/ICL/Research Project/Data/Tree Ring Width/Image section")

img_files<-list.files(path=getwd(), pattern = ".*.tif$", full.names = TRUE)

###############################################################################
##Function ringDetect: detection of ring borders and plotting of image segments. 


## Plot 2 #####################################################################
## 683_1411_1
plot21 <- ringDetect (image = img_files[36], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.4, ppi = 600)
Width21 <- ringWidths (image = img_files[36], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.4, ppi = 600)

## 682_1410_1
plot22 <- ringDetect (image = img_files[35], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.35, exclu = c(1934,2250,2275), ppi = 600)
Width22 <- ringWidths (image = img_files[35], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.35, exclu = c(1934,2250,2275), ppi = 600)

## 680_1408_1
plot23 <- ringDetect (image = img_files[34], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.5, ppi = 600)
Width23 <- ringWidths (image = img_files[34], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.5, ppi = 600)

## 1372_2397_1
plot24 <- ringDetect(image = img_files[15], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.7, ppi = 600)
Width24 <- ringWidths(image = img_files[15], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.7, ppi = 600)

## 1681_2807_1
plot25 <- ringDetect (image = img_files[19], rgb = c(0.9, 0, 0.1), last.yr = 2023, darker = T, exclu = c(1,60,2136), ppi = 600,)
Width25 <- ringWidths (image = img_files[19], rgb = c(0.9, 0, 0.1), last.yr = 2023, darker = T, exclu = c(1,60,2136), ppi = 600,)

## 1704_2836_1
plot26 <- ringDetect (image = img_files[21], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.6, exclu = c(532,636), ppi = 600)
Width26 <- ringWidths (image = img_files[21], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.6, exclu = c(532,636), ppi = 600)

## 1702_2834_1
plot27 <- ringDetect (image = img_files[20], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.2, exclu = c(1186), inclu = c(25,1220), ppi = 600)
Width27 <- ringWidths (image = img_files[20], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.2, exclu = c(1186), inclu = c(25,1220), ppi = 600)

## 1541_2606_1
plot28 <- ringDetect (image = img_files[16], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.2, exclu = c(27,1364,1912), ppi = 600)
Width28 <- ringWidths (image = img_files[16], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.2, exclu = c(27,1364,1912), ppi = 600)

## 1543_5650_1
plot29 <- ringDetect (image = img_files[17], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.5, exclu = c(1), inclu = c(30), ppi = 600)
Width29 <- ringWidths (image = img_files[17], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.5, exclu = c(1), inclu = c(30), ppi = 600)

## 1680_5630_1
plot30 <- ringDetect (image = img_files[18], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.3, exclu = c(1), ppi = 600)
Width30 <- ringWidths (image = img_files[18], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = T, p.row = 0.3, exclu = c(1), ppi = 600)



## Plot 3 #####################################################################

## 662_1387_1
plot31 <- ringDetect (image = img_files[31], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.75, exclu = c(1500,1592,1802,1936,2028), ppi = 600)
Width31 <- ringWidths (image = img_files[31], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.75, exclu = c(1500,1592,1802,1936,2028), ppi = 600)

## 83_106_1
plot32 <- ringDetect (image = img_files[38], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.75, exclu = c(304,1079,1401,1434,1696,1754,1957), inclu = c(2650), ppi = 600)
Width32 <- ringWidths (image = img_files[38], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.75, exclu = c(304,1079,1401,1434,1696,1754,1957), inclu = c(2650), ppi = 600)

## 81_104_1
plot33 <- ringDetect (image = img_files[37], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.5, exclu = c(1875,2081,2431,2735,3439), ppi = 600)
Width33 <- ringWidths (image = img_files[37], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.5, exclu = c(1875,2081,2431,2735,3439), ppi = 600)

## 98_127_1
plot34 <- ringDetect (image = img_files[40], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.75, exclu = c(1893,2120,2400,2433,2578,2787,2947,3104), ppi = 600)
Width34 <- ringWidths (image = img_files[40], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.75, exclu = c(1893,2120,2400,2433,2578,2787,2947,3104), ppi = 600)

## 96_125_1
plot35 <- ringDetect (image = img_files[39], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.2, exclu = c(1900,2094,2269,2501,2561), inclu = c(2790), ppi = 600)
Width35 <- ringWidths (image = img_files[39], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.2, exclu = c(1900,2094,2269,2501,2561), inclu = c(2790), ppi = 600)

## 2866_122_1
plot36 <- ringDetect (image = img_files[22], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.75, exclu = c(1469,2842,3105,3360,3468), ppi = 600)
Width36 <- ringWidths (image = img_files[22], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.75, exclu = c(1469,2842,3105,3360,3468), ppi = 600)

## 67_82_1
plot37 <- ringDetect (image = img_files[32], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.2, exclu = c(21,649,855,889,1085,1065,1082), ppi = 600)
Width37 <- ringWidths (image = img_files[32], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.2, exclu = c(21,649,855,889,1085,1065,1082), ppi = 600)

## 68_84_1
plot38 <- ringDetect (image = img_files[33], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.2, exclu = c(1,795,896,1028,1067,1081,1103,1199,1212,1252,1312,1369,1394,1409,1454), ppi = 600)
Width38 <- ringWidths (image = img_files[33], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.2, exclu = c(1,795,896,1028,1067,1081,1103,1199,1212,1252,1312,1369,1394,1409,1454), ppi = 600)

## 626_1340_1
plot39 <- ringDetect (image = img_files[29], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.3, exclu = c(1225), inclu = c(1520), ppi = 600)
Width39 <- ringWidths (image = img_files[29], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.3, exclu = c(1225), inclu = c(1520), ppi = 600)

## 629_1344_1
plot40 <- ringDetect (image = img_files[30], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.7, exclu = c(27,1114,1701,1843,1978,3413,3539,3709,3880,4056,4194), ppi = 600)
Width40 <- ringWidths (image = img_files[30], rgb = c(0.9, 0, 0.1), origin = 0, last.yr = 2023, darker = F, p.row = 0.7, exclu = c(27,1114,1701,1843,1978,3413,3539,3709,3880,4056,4194), ppi = 600)



## create a list to merge all data.frame (Width) ##############################

Ring_Widths_Plot2 <- list (Width21, Width22, Width23, Width24, Width25, Width26, Width27, Width28, Width29, Width30)

save (Ring_Widths_Plot2, file = "Ring_Widths_Plot2.RData")

Ring_Widths_Plot3 <- list (Width31, Width32, Width33, Width34, Width35, Width36, Width37, Width38, Width39, Width40)

save (Ring_Widths_Plot3, file = "Ring_Widths_Plot3.RData")

Ring_Widths_Plot <- list (Width1, Width2, Width3, Width4, Width5, Width6, Width7, Width8, Width9, Width10, 
                          Width11, Width12, Width13, Width14, Width15, Width16, Width17, Width18, Width19, Width20,
                          Width21, Width22, Width23, Width24, Width25, Width26, Width27, Width28, Width29, Width30,
                          Width31, Width32, Width33, Width34, Width35, Width36, Width37, Width38, Width39, Width40)

save (Ring_Widths_Plot, file = "Ring_Widths_Plot.RData")

## do some plots ##############################################################

plot (Width23)

