# This file contains 3 functions for use in estimating a time series of T model
# DBH and delta DBH values backwards in time.
# 
# tmodel_delta_dbh:
# 
# This function is taken from Guangqi Li's original R code for the T Model
# paper. It simply returns the estimated delta DBH given the initial DBH and
# model parameters.
# 
# delta_dbh_uniroot_function:
# 
# The uniroot function requires a function that takes an input value and returns
# a single value that is expected to change sign at some point in a provided
# interval. This function implements (dbh_t + delta_dbh_t) - dbh_t+1 and returns
# that value - uniroot can then vary the initial argument (dbh) and find a dbh
# value that returns zero (to some degree of precision).
# 
# reverse_predict_dbh_delta_dbh:
# 
# This function simply implements the uniroot function. It takes a dbh_t+1 value
# and all the other parameters and returns the calculated dbh and delta_dbh such
# that (dbh_t + delta_dbh_t) = dbh_t+1.


########## T model part
tmodel_delta_dbh <- function(
    dbh, GPP, L, a, cr, Hm, rho, rr, rs, zeta, y, sigma, tf, tr, K) {

    # This code is taken directly from Guangqi Li's original R function

  # P0: potential annual GPP (P model). 10%: foliage matainance respiration
  GPP <- GPP * (1 - 0.1)
  # H is controlled by d, and constrained by maximum height (Hm)
  H <- Hm * (1 - exp(-a * dbh / Hm))
  # crown ratio
  fc <- H / (a * dbh)
  # crown area
  Ac <- ((pi * cr) / (4 * a)) * dbh * H
  # stem mass
  Ws <- (pi / 8) * (dbh^2) * H * rho
  # foliage mass
  Wf <- Ac * L * (sigma^(-1))
  # sapwood mass
  Wss <- Ac * rho * H * (1 - fc / 2) / cr
  # GPP captured by crown
  GPP <- Ac * GPP
  # GPP fixed per m2 of crown
  gpp <- GPP * (1 - exp(-(K * L)))
  # sapwood respiration
  Rm1 <- Wss * rs
  # fine root respiration
  Rm2 <- zeta * sigma * Wf * rr
  # NPP after multiplied by the yeild factor
  NPP1 <- y * (GPP - Rm1 - Rm2)
  # turnover of foliage and fine root
  NPP2 <- (Ac * L * ((1 / (sigma * tf)) + (zeta / tr)))

  # increment of diameter dD
  num <- y * (gpp - rho * (1 - H / (2 * a * dbh))
    * H * rs / cr - L * zeta * rr
  ) - L * (1 / (sigma * tf) + zeta * (1 / tr))
  den <- (a / (2 * cr)) *
    rho * (a * dbh * (1 / H - 1 / Hm) + 2) +
    (L / dbh) * (a * dbh * (1 / H - 1 / Hm) + 1) *
      (1 / sigma + zeta)

  dD <- num / den

  # increment of wood dWs/dt
  dWs <- (pi / 8 * rho * dbh * (a * dbh * (1 - (H / Hm)) + 2 * H)) * dD
  # increament of foliage and fine root
  dWfr <- (L * ((pi * cr) / (4 * a)) *
    (a * dbh * (1 - (H / Hm) + H)) * (1 / sigma + zeta)
  ) * dD

  return(dD)
}

delta_dbh_uniroot_function <- function(
    dbh_t, dbh_tplus1, GPP, L, a, cr, Hm, rho,
    rr, rs, zeta, y, sigma, tf, tr, K) {
  # This function is designed to be used with uniroot - uniroot will
  # adjust the first argument (dbh_t) until it finds a value where the
  # return value is zero.

  # Returns (dbh_t + delta_dbh_t) - dbh_tplus1 for a given dbh

  dD <- tmodel_delta_dbh(
    dbh_t, GPP, L, a, cr, Hm, rho, rr, rs, zeta, y, sigma, tf, tr, K
  )

  return((dbh_t + dD) - dbh_tplus1)
}


reverse_predict_dbh_delta_dbh <- function(
    dbh_tplus1, GPP, L, a, cr, Hm, rho, rr,
    rs, zeta, y, sigma, tf, tr, K) {
  # Normally, the tmodel takes dbh at time t and finds delta_dbh
  # for time t and hence dbh at time t+1:
  #
  #  dbh_t+1 = dbh_t + delta_dbh_t
  #
  # Here, we know dbh_t+1 and the conditions (GPP etc) for time t,
  # but not dbh_t. So we re-express this as:
  #
  #  (dbh_t + delta_dbh_t) - dbh_t+1  = 0
  #
  # We can then use a root solver (uniroot), to numerically find dbh_t
  # that satisfies the equation above.

  dbh_solved <- uniroot(
    f = delta_dbh_uniroot_function, interval = c(0.0001, dbh_tplus1), tol = 1e-9,
    dbh_tplus1 = dbh_tplus1, GPP = GPP, L = L, a = a, cr = cr,
    Hm = Hm, rho = rho, rr = rr, rs = rs, zeta = zeta, y = y,
    sigma = sigma, tf = tf, tr = tr, K = K
  )

  delta_dbh <- tmodel_delta_dbh(
    dbh_solved$root, GPP, L, a, cr, Hm, rho, rr, rs, zeta, y, sigma, tf, tr, K
  )

  return(c(dbh_solved$root, delta_dbh))
}
