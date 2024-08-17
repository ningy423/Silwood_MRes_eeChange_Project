library(xts)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(cowplot)

# 01.load the monthly xts data
########################################################################
setwd("D:/ICL/Research Project/Data/Environmental data/Silwood_data_to_curate")
load("derived_data.RData")
setwd("D:/ICL/Research Project/Data/GPP")
load("monthly_pmodel.RData")

# 02. calculate yearly environment variables
###############################################################################

# 02.01. the defined growing season is from April to October
# define a function of subset certain months and do statistics
average_apr_oct <- function(x) {
  subset <- x[format(index(x), "%m") %in% c("04", "05", "06", "07", "08", "09", "10")]
  return(mean(subset, na.rm = TRUE))
}

max_apr_oct <- function(x) {
  subset <- x[format(index(x), "%m") %in% c("04","05","06","07","08","09","10")]
  return(max(subset,na.rm = TRUE))
}

min_apr_oct <- function(x) {
  subset <- x[format(index(x), "%m") %in% c("04","05","06","07","08","09","10")]
  return(min(subset,na.rm = TRUE))
}

sum_apr_oct <- function(x) {
  subset <- x[format(index(x), "%m") %in% c("04","05","06","07","08","09","10")]
  return(sum(subset,na.rm = TRUE))
}

# aggregate monthly environmental indices to YEARLY as a data.frame "environment_df" 
environment_list <- list()
environment_list $ tc_max <- apply.yearly (data_ts$MAX.Temperature, max_apr_oct)
environment_list $ tc_min <- apply.yearly (data_ts$MIN.Temperature, min_apr_oct)
environment_list $ tc <- apply.yearly (data_ts$Avg.Temperature, average_apr_oct)
environment_list $ pn <- apply.yearly (data_ts$Total.Rainfall..mm., sum_apr_oct)
environment_list $ RH <- apply.yearly (data_ts$Relative.Air.Humidity...., average_apr_oct)
environment_list $ sw_in <- apply.yearly (data_ts$Average.Solar.Radiation..W.m2., average_apr_oct)
environment_list $ fapar <- apply.yearly (data_ts$fAPAR...., average_apr_oct)
environment_list $ co2 <- apply.yearly (data_ts$CO2, average_apr_oct)
environment_list $ soilm <- apply.yearly (data_ts$Soil.Moisture, average_apr_oct)
environment_list $ vpd <- apply.yearly (data_ts$vpd, average_apr_oct)
environment_list $ meanalpha <- apply.yearly (data_ts$alpha, average_apr_oct)
environment_list $ ppfd <- apply.yearly (data_ts$ppfd, sum_apr_oct)

environment_df <- data.frame(Year = as.integer(format(index(environment_list$tc_max), "%Y")))

# convert each xts object as data frames, and merge by Year
for (i in seq_along(environment_list)) {
  
  # abstract indexes and value of current objects
  xts_index <- index(environment_list[[i]])
  xts_values <- as.data.frame(environment_list[[i]])
  
  # create a temporal data frame containing Year and values
  df_temp <- data.frame(Year = as.integer(format(xts_index, "%Y")), xts_values)
  
  # merge df_temp into environment_df by Year
  environment_df <- merge(environment_df, df_temp, by = "Year", all = TRUE)
}
colnames(environment_df) <- c("Year","tc_max","tc_min","tc","pn","RH","sw_in","fapar","co2","soilm","vpd","meanalpha","ppfd")
# calculate LAI
environment_df$LAI <- log(1-environment_df$fapar)/(-0.5)
setwd("D:/ICL/Research Project/Data/Environmental data/Silwood_data_to_curate")
save(environment_df,file = "environment_df.RData")


# 03. calculate yearly GPP
########################################################################
# 03.01. convert monthly_pmodel to annual_gpp_df by the growing season defined from April to October
# use apply.yearly to calculate average gpp
yearly_gpp <- apply.yearly(monthly_pmodel$gpp/1000, sum_apr_oct)

# convert to data.frame
yearly_gpp_df <- data.frame(Date = index(yearly_gpp), Value = coredata(yearly_gpp))

# abstract Year
yearly_gpp_df <- yearly_gpp_df %>%
  dplyr::mutate(Year = year(Date)) %>%
  #dplyr::filter(Year != min(Year), Year != max(Year)) %>%
  dplyr::select(Year, gpp)

setwd("D:/ICL/Research Project/Data/GPP")
save(yearly_gpp_df,file = "yearly_gpp_df.RData")


# 04. Merge 2 data frames: environment_df, yearly_gpp_df
############################################################################

#setwd("D:/ICL/Research Project/Data/Tree Ring Width/Ring Widths Observation")
#Ring_Widths_Observation_df <- read.csv("Ring_Widths_df.csv")
#colnames(Ring_Widths_Observation_df) <- c("Year","rw_observation_mm","age_observation","ID","Plot")

#setwd("D:/ICL/Research Project/Data/Tree Ring Width/Ring Widths Simulation")
#load("Ring_Widths_Simulation_df5.RData")
#Ring_Widths_Simulation <- final_results %>% arrange(ID, Year) %>% group_by(ID) %>% mutate(age_simulation = row_number())
#colnames(Ring_Widths_Simulation) <- c("ID", "Year","D_m","H_m","Ac_m2","tree_GPP_kg","dD_dt", "dH_dt", "dWs_dt","rw_simulation_mm","age_simulation")

#Merge2 <- merge (Ring_Widths_Observation_df, Ring_Widths_Simulation, by = c("Year","ID"), all = TRUE)
# in Merge, each environment indicator and gpp will repeat according to the number of tree rings per year
#Merge <- merge (Merge1, Merge2, by = "Year", all = TRUE)

setwd("D:/ICL/Research Project/Data/Merge")
colnames(yearly_gpp_df) <- c("Year","yearly_GPP_kg_m2")
save (environment_df, file = "environment_df.RData")
save (yearly_gpp_df, file = "yearly_gpp_df.RData")
#save (Ring_Widths_Observation_df, file = "Ring_Widths_Observation_df.RData")
#save (Merge, file = "Merge.RData")


# 05. plot biophysical environment data: the climate change from 1987 to 2023
#############################################################################

biophysical_xts <- merge(data_ts, monthly_pmodel)
colnames(biophysical_xts) <- c("tc_max","tc_min","tc","pn","RH","sw_in","fapar","lat","elev","soilm","co2","es","ea","vpd","ppfd","meanalpha",
                               "gpp","ca","gammastar","kmm","ns_star","chi","xi","mj","mc","ci","iwue","gs","vcmax","vcmax25","jmax","jmax25","rd")

biophysical_monthly_df <- data.frame(Date = index(biophysical_xts), coredata(biophysical_xts))
biophysical_monthly_df$gpp <- biophysical_monthly_df$gpp/1000
biophysical_monthly_df$Year <- as.numeric(format(biophysical_monthly_df$Date, "%Y"))
#biophysical_monthly_df$Month <- format(biophysical_monthly_df$Date, "%m")
biophysical_monthly_df$Type <- "Monthly"
biophysical_monthly_df$Date <- as.Date(biophysical_monthly_df$Date)

biophysical_df <- merge (environment_df, yearly_gpp_df, by = "Year")
colnames(biophysical_df) <- c("Year","tc_max","tc_min","tc","pn","RH","sw_in","fapar","co2","soilm","vpd","meanalpha","ppfd","LAI","gpp")
biophysical_df$Year <- as.numeric(biophysical_df$Year)
biophysical_df$Type <- "Yearly"
biophysical_df$Date <- as.Date(paste0(biophysical_df$Year, "-01-01"))

# plot environmental variables

selected_vars <- c("tc","vpd", "soilm", "meanalpha", "ppfd", "fapar", "co2", "gpp")

monthly_long <- biophysical_monthly_df %>%
  select(Date, all_of(selected_vars)) %>%
  pivot_longer(cols = -Date, names_to = "Variable", values_to = "Value") %>%
  mutate(Type = "Monthly", Year = as.numeric(format(Date, "%Y")))

yearly_long <- biophysical_df %>%
  select(Year, Date, all_of(selected_vars)) %>%
  pivot_longer(cols = -c(Year, Date), names_to = "Variable", values_to = "Value") %>%
  mutate(Type = "Yearly")

combined_long <- bind_rows(monthly_long, yearly_long)

combined_long$Variable <- factor(combined_long$Variable, levels = selected_vars)

combined_long <- combined_long %>%
  mutate(Date = as.Date(Date),
         Year = as.numeric(Year),
         Variable = factor(Variable, levels = unique(Variable)))

variable_names <- c(
  "tc" = "(a) TC (℃)",
  "vpd" = "(b) VPD (Pa)",
  "soilm" = "(c) SOILM (0-1)",
  "meanalpha" = "(d) α (0-1)",
  "ppfd" = "(e) PPFD (mol/m2/month)",
  "fapar" = "(f) fAPAR (0-1)",
  "co2" = "(g) [CO2] (ppm)",
  "gpp" = "(h) GPP (kg/m2)"
)


ggplot(combined_long) +
  geom_line(data = filter(combined_long, Type == "Monthly"), aes(x = Date, y = Value, color = "Monthly", group = interaction(Variable, Year)), lwd = 1, alpha=0.5) +
  geom_line(data = filter(combined_long, Type == "Yearly"), aes(x = as.Date(paste0(Year, "-01-01")), y = Value, color = "Yearly", group = Variable), lwd = 1) +
  geom_point(data = filter(combined_long, Type == "Yearly"), aes(x = as.Date(paste0(Year, "-01-01")), y = Value, color = "Yearly"), lwd = 1.5) +
  geom_smooth(data = filter(combined_long, Type == "Yearly"), aes(x = as.Date(paste0(Year, "-01-01")), y = Value, color = "smooth"), method = loess, lwd = 1.5, lty = 1.5) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  labs( x = "Year", y = "Value") +
  scale_color_manual(values = c("Monthly" = "#DAA520", "Yearly" = "#4169E1", "smooth" = "#0000FF")) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 4, labeller = labeller(Variable = variable_names, size=12)) +  
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "black"),  
        axis.ticks = element_line(color = "black"), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "noun"
  )



# overall linear trend
summary(lm(biophysical_df$tc ~ biophysical_df$Year))
summary(lm(biophysical_df$vpd ~ biophysical_df$Year))
summary(lm(biophysical_df$soilm ~ biophysical_df$Year))
summary(lm(biophysical_df$meanalpha ~ biophysical_df$Year))
summary(lm(biophysical_df$ppfd ~ biophysical_df$Year))
summary(lm(biophysical_df$co2 ~ biophysical_df$Year))
# set a breakpoint in 2000
summary(lm(vpd ~ Year, data = biophysical_df [biophysical_df$Year <=2000,]) )
summary(lm(vpd ~ Year, data = biophysical_df [biophysical_df$Year >2000,]) )
summary(lm(soilm ~ Year, data = biophysical_df [biophysical_df$Year <=2014,]))
summary(lm(soilm ~ Year, data = biophysical_df [biophysical_df$Year >2014,]))
summary(lm(meanalpha ~ Year, data = biophysical_df [biophysical_df$Year <=2005,]))
summary(lm(meanalpha ~ Year, data = biophysical_df [biophysical_df$Year >2005,]))
summary(lm(ppfd ~ Year, data = biophysical_df [biophysical_df$Year < 2019, ]))
summary(lm(ppfd ~ Year, data = biophysical_df [biophysical_df$Year >= 2019, ]))
summary(lm(fapar ~ Year, data = biophysical_df))

mean(biophysical_df$gpp)
min(biophysical_monthly_df$gpp)
summary(lm(biophysical_df$yearly_GPP_kg_m2 ~ biophysical_df$Year))
summary(lm(data = biophysical_df, gpp ~ tc + vpd + soilm + meanalpha + fapar + ppfd + co2))
