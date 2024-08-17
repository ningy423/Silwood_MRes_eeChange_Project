library(ggplot2)
library(tidyr)
library(dplyr)
library(stats)
#library(zoo)
library(LSD)
#library(broom)
library(gridExtra)
#library(MASS)
#library(car)
library(visreg)

setwd("D:/ICL/Research Project/Data/Merge")
load("Merge1.RData") # first simulation
Merge1 <- fulldb



# 01.Comparison of the Mean, interannual variability, and RMSE of observed and simulated rings
############################################################################################################
Merge_long <- Merge1 %>%
  pivot_longer(cols = c(rw_obs, rw_sim1, rw_sim2), 
               names_to = "Method", 
               values_to = "rw_mm")

#Merge_long <- Merge_long %>%
 # pivot_longer(cols = c(age_observation,age_simulation),
  #             names_to = "Method2",
   #            values_to = "Age")

ggplot(Merge_long, aes(x = as.factor(Year), y = rw_mm, fill = Method), ylim = c(0,20)) +
  geom_boxplot() +
  labs(x = "Year", y = "Ring width (mm)") +
  theme_minimal() +
  #scale_color_manual(values = c("rw_obs_mm" = "#8B4513", "rw_sim_mm" = "#00008B"),
   #                 labels = c("rw_obs_mm"="Observations", "rw_sim_mm"="Simulations")) +
  scale_fill_manual(values = c("rw_obs" = "#FFD700", "rw_sim1" = "#87CEEB", "rw_sim2" = "#4169E1"),
                labels = c("rw_obs"="Observations", "rw_sim1" = "Simulation 1", "rw_sim2" = "Simulation 2")) +
  scale_x_discrete(breaks = seq(1987, 2023, by = 3)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.5),  
    axis.ticks = element_line(color = "black", linewidth = 0.5), 
    panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.85, 0.85)
  )

obs_width <- Merge1 %>% group_by(Year) %>% summarise(Mean=mean(rw_obs,na.rm=T), SD=sd(rw_obs,na.rm=T))
sim_width1 <- Merge1 %>% group_by(Year) %>% summarise(Mean=mean(rw_sim1,na.rm=T), SD=sd(rw_sim1,na.rm=T))
sim_width2 <- Merge1 %>% group_by(Year) %>% summarise(Mean=mean(rw_sim2,na.rm=T), SD=sd(rw_sim2,na.rm=T))

ggplot() +
  geom_line(data = obs_width, aes(x = Year, y = Mean, color = "Observation"), size = 1.5) +
  geom_errorbar(data = obs_width, aes( x= Year, ymin = Mean-SD, ymax = Mean+SD, color = "Observation"),size=0.8) +
  geom_line(data = sim_width1, aes(x = Year, y = Mean, color = "Simulation with original a, c, and ζ"), size=1.5, lty=1.5) +
  geom_line(data = sim_width2, aes(x = Year, y = Mean, color = "Simulation with optimized a, c, and ζ"), size = 1.5) +
  geom_errorbar(data = sim_width2, aes(x = Year, ymin = Mean-SD, ymax = Mean+SD, color = "Simulation with optimized a, c, and ζ"),size=0.8) +
  scale_color_manual(values = c("Observation" = "#DAA520","Simulation with original a, c, and ζ" = "#87CEEB", "Simulation with optimized a, c, and ζ" = "#4169E1")) +
  labs(x = "Year", y = "Mean ring width (mm)", color = "Method") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1987,2023,by=3)) +
  scale_y_continuous(breaks = seq(0,10,by=1)) +
  coord_cartesian(ylim = c(0, 10)) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(size = 10), 
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.75, 0.85)
  )

heatscatter(Merge1$rw_obs, Merge1$rw_sim2, xlab = "Observed Ring Widths (mm)", ylab = "Simulated Ring Widths (mm)", title = "Heatscatter", xlim = c(0,15), ylim = c(0,15))
lm_rw <- lm(Merge1$rw_sim2 ~ Merge1$rw_obs-1)
lm_summary <- summary(lm_rw)
R2 <- lm_summary$r.squared
RMSE <- sqrt(mean(lm_summary$residuals^2))
Slope <- coef(lm_summary)[1]
abline(lm_rw, col = "black", lwd = 2)
abline(0, 1, col = "black", lwd = 1, lty = 2)
legend("topleft", legend = paste("R^2 =", round(R2, 2), "\nRMSE =", round(RMSE, 2), "\nSlope =", round(Slope,2)),
       bty = "n", cex = 1, text.col = "black")



############################################################################################################
# 02. Multiple linear regressions for obs and sim as functions of bioclimate
############################################################################################################

#################################################################################
# 02.01. general Muptiple Linear Regression with log_Age(previous version)

Merge1$log_Age <- log(Merge1$Age)
lm_obs <- lm (data = Merge1, rw_obs ~ tc + soilm + vpd + meanalpha + ppfd + co2 + log_Age)
summary(lm_obs)
#step_model <- step(lm_obs, direction = "backward", trace = TRUE)
#anova(step_model, lm_obs)
#lm_obs <- lm (data = Merge, rw_obs_mm ~ soilm + vpd + ppfd + co2 + Age)

lm_sim <- lm (data = Merge1, rw_sim2 ~ tc + soilm + vpd + meanalpha + ppfd + co2 + log_Age)
summary(lm_sim)
#step_model <- step(lm_sim, direction = "backward", trace = TRUE)
#anova(step_model, lm_sim)
#lm_sim <- lm (data = Merge, rw_sim_mm ~ vpd + ppfd + co2)


#################################################################################
# 02.02 put together obs and sim, using natural logrithm for Age (FINAL)

# Function to calculate slope and p-value for each variable
calculate_metrics <- function(model, variable) {
  summary_model <- summary(model)
  coef_summary <- summary_model$coefficients
  slope <- coef_summary[variable, "Estimate"]
  p_value <- coef_summary[variable, "Pr(>|t|)"]
  return(list(slope = slope, p_value = p_value))
}


plot_partial_residuals <- function(model_obs, model_sim, variables) {
  plots <- list()
  for (variable in variables) {
    metrics_obs <- calculate_metrics(model_obs, variable)
    metrics_sim <- calculate_metrics(model_sim, variable)
    
    visreg_var <- if (variable == "log(Age)") "log_Age" else variable
    
    p_obs <- visreg(model_obs, visreg_var, type = "conditional", plot = FALSE)
    p_sim <- visreg(model_sim, visreg_var, type = "conditional", plot = FALSE)
    
    plot <- ggplot() +
      geom_point(data = p_obs$res, aes_string(x = visreg_var, y = "visregRes"), color = "#DAA520", alpha = 0.5, pch=1) +
      geom_smooth(data = p_obs$res, aes_string(x = visreg_var, y = "visregRes"), method = "lm", se = TRUE, color = "#DAA520", size=1.5) +
      geom_point(data = p_sim$res, aes_string(x = visreg_var, y = "visregRes"), color = "#4169e1", alpha = 0.5, pch=1) +
      geom_smooth(data = p_sim$res, aes_string(x = visreg_var, y = "visregRes"), method = "lm", se = TRUE, color = "#4169e1", size=1.5) +
      labs(x = variable, y = "Ring Width (mm)") +
      theme_classic() +
      theme(
        #legend.position = "none",
        axis.text = element_text(size = 20), 
        axis.title = element_text(size = 20),
        panel.border = element_rect(linewidth = 1, fill = NA)
      ) +
      annotate("text", x = Inf, y = Inf, label = "Observation", hjust = 1.1, vjust = 1.5, size = 7, color = "#DAA520") +
      annotate("text", x = Inf, y = Inf, label = "Simulation",  hjust = 1.1, vjust = 3, size = 7, color = "#4169e1") 
    
    plots[[variable]] <- plot
  }
  return(plots)
}

variables <- c("tc", "soilm", "vpd", "meanalpha", "ppfd", "co2", "log_Age")

# Generate partial residual plots
plots <- plot_partial_residuals(lm_obs, lm_sim,  variables)

# Arrange plots
do.call(grid.arrange, c(plots, ncol = 3))


#################################################################################
# 02.03 put together obs, sim1, and sim2, using natural logrithm for Age (FINAL)

plot_partial_residuals <- function(model_obs, model_sim1, model_sim2, variables) {
  plots <- list()
  for (variable in variables) {
    metrics_obs <- calculate_metrics(model_obs, variable)
    metrics_sim1 <- calculate_metrics(model_sim1, variable)
    metrics_sim2 <- calculate_metrics(model_sim2, variable)
    
    visreg_var <- if (variable == "log(Age)") "log_Age" else variable
    
    p_obs <- visreg(model_obs, visreg_var, type = "conditional", plot = FALSE)
    p_sim1 <- visreg(model_sim1, visreg_var, type = "conditional", plot = FALSE)
    p_sim2 <- visreg(model_sim2, visreg_var, type = "conditional", plot = FALSE)
    
    plot <- ggplot() +
      geom_point(data = p_obs$res, aes_string(x = visreg_var, y = "visregRes"), color = "#DAA520", alpha = 0.5, pch=1) +
      geom_smooth(data = p_obs$res, aes_string(x = visreg_var, y = "visregRes"), method = "lm", se = TRUE, color = "#DAA520", size=1.5) +
      geom_point(data = p_sim1$res, aes_string(x = visreg_var, y = "visregRes"), color = "#87CEEB", alpha = 0.5, pch=1) +
      geom_smooth(data = p_sim1$res, aes_string(x = visreg_var, y = "visregRes"), method = "lm", se = TRUE, color = "#87CEEB", size=1.5) +
      geom_point(data = p_sim2$res, aes_string(x = visreg_var, y = "visregRes"), color = "#4169e1", alpha = 0.5, pch=1) +
      geom_smooth(data = p_sim2$res, aes_string(x = visreg_var, y = "visregRes"), method = "lm", se = TRUE, color = "#4169e1", size=1.5) +
      labs(x = variable, y = "Ring Width (mm)") +
      theme_classic() +
      theme(
        #legend.position = "none",
        axis.text = element_text(size = 20), 
        axis.title = element_text(size = 20),
        panel.border = element_rect(linewidth = 1, fill = NA)
      ) +
      annotate("text", x = Inf, y = Inf, label = "Observation", hjust = 1.1, vjust = 1.5, size = 7, color = "#DAA520") +
      annotate("text", x = Inf, y = Inf, label = "Simulation 1",  hjust = 1.1, vjust = 3, size = 7, color = "#87CEEB") +
      annotate("text", x = Inf, y = Inf, label = "Simulation 2",  hjust = 1.1, vjust = 4.5, size = 7, color = "#4169e1")
    
    plots[[variable]] <- plot
  }
  return(plots)
}

variables <- c("tc", "soilm", "vpd", "meanalpha", "ppfd", "co2", "log_Age")

# Generate partial residual plots
plots <- plot_partial_residuals(lm_obs, lm_sim1, lm_sim2, variables)

# Arrange plots
do.call(grid.arrange, c(plots, ncol = 3))
