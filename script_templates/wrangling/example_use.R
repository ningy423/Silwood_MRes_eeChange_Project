# This file implements the calculation of a time series for DBH and delta DBH
# backwards in time from a DBH measurement. It is set up to cycle over a time
# series of GPP and LAI.



# Load functions
source("t_model_reverse.r")

# Constants from original paper
a_hd <- a
ca_ratio <- c
h_max <- Hm # H_m, Maximum tree height (m)
rho_s <- 538.0 # rho_s, Sapwood density (kgCm-3)
sla <- 25.08 # sigma, Specific leaf area (m2 kg-1C)
tau_f <- 1.0 # tau_f, Foliage turnover time (years)
tau_r <- 1.27 # tau_r, Fine-root turnover time (years)
par_ext <- 0.5 # k, PAR extinction coefficient (-)
yld <- 0.6 # y, Yield_factor (-)
zeta <- 0.17 # zeta, Ratio of fine-root mass to foliage area (kgCm-2)
resp_r <- 0.913 # r_r, Fine-root specific respiration rate (year-1)
resp_s <- 0.0419 # r_s, Sapwood-specific respiration rate (year-1)
resp_f <- 0.1 # --- , Foliage maintenance respiration fraction (-)

# Data - dummy constant time series
trees <- Oak
dbh_2023 <- Oak$D_m
n_year <- sort (yearly_gpp_df$Year, decreasing = T)
gpp_time_series <- yearly_gpp_df$gpp
lai_time_series <- environment_df$LAI

# Set up the DBH and delta DBH stores
dbh <- numeric(nrow(trees))
delta_dbh <- numeric(nrow(trees))

for (idx in seq_len(n_year)) {
  # get the previous year estimates of dbh and delta_dbh
  previous_year <- reverse_predict_dbh_delta_dbh(
    dbh_tplus1 = dbh[idx],
    GPP = gpp_time_series[idx],
    L = lai_time_series[idx],
    a = a_hd, cr = ca_ratio, Hm = h_max, rho = rho_s, rr = resp_r,
    rs = resp_s, zeta = zeta, y = yld, sigma = sla, tf = tau_f,
    tr = tau_r, K = par_ext
  )

  dbh[idx + 1] <- previous_year[1]
  delta_dbh[idx] <- previous_year[2]
}

# Check the calculations - (dbh_t + delta_dbh_t) should equal dbh_t+1 to the
# precision limit of the uniroot finder.

(dbh[-1] + delta_dbh) - dbh[-(n_year + 1)]
