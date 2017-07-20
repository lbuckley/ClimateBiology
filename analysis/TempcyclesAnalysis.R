#' Generate simulated weather record.
#'
#' Generate time series of temperature measurements
#'
#' @param timebase A vector of dates / times in decimal julian day. Either this
#'   or \code{years} must be supplied.
#' @param years A scalar, the number of years to model. Either this or
#'   \code{timebase} must be supplied.
#' @param spectrum A data frame of \code{cbind(frequency, cyc_range, phase,
#'   tau)}. Day frequency = 1, year frequency = 1/325.25. Cycling range = 2 *
#'   amplitude. Phase is -pi to pi, as output by nlts_plus_phase. tau is lag,
#'   out output by nlts_plus_phase.
#' @param mean The mean temperature. Use \code{t_intercept} for trended data.
#'   Default 0.
#' @param t_int T intercept for use with linear trend.
#' @param t_slope Slope for use with linear trend.
#' @param mean_resid Mean residual. Currently rnorm with sd = 1, without
#'   autocorrelation. Next version will have ARIMA autocorrelation.
#' @return a numeric vector of the number of days since 1/1/1960, dates before
#'   this are negative.
#' @export
#' @examples
#' library(ggplot2)
#' tropical_spectrum <- data.frame(frequency = c(1, 1 / 365),
#'                                 cyc_range = c(6.53, 4.1),
#'                                 phase     = c(0.421,0.189),
#'                                 tau       = c(0,0))
#' tropical_mean <- 25.845
#' n_temperate_spectrum <- data.frame(frequency = c(1, 1 / 365),
#'                                    cyc_range = c(5.33, 26.36),
#'                                    phase     = c(0.319,1.057),
#'                                    tau       = c(0,0))
#' n_temperate_mean <- 10.457
#' tropical_ts <- gen_cycling_rec(years    = 2,
#'                                spectrum = tropical_spectrum,
#'                                mean     = tropical_mean)
#' tropical_ts$region <- "Tropical"
#' n_temperate_ts <- gen_cycling_rec(years    = 2,
#'                                   spectrum = n_temperate_spectrum,
#'                                   mean     = n_temperate_mean)
#' n_temperate_ts$region <- "North Temperate"
#' ts_data <- rbind(tropical_ts,
#'                  n_temperate_ts)
#' ts_data <- factor(ts_data$region,
#'                   levels = c("Tropical",
#'                              "North Temperate"))
#' ggplot(data = ts_data,
#'        aes(x = jday,
#'            y = temperature,
#'            color = region)) +
#'    geom_line(alpha = 0.7)
gen_cycling_rec <- function(timebase   = NULL,
                            years      = NULL,
                            spectrum = stop("data frame of data.frame(frequency, cyc_range, phase, tau) required"),
                            mean       = 0,
                            t_int      = NULL,
                            t_slope    = NULL,
                            mean_resid = NULL) {
  if (is.null(timebase)) {
    if (is.null(years)) {stop("Either timebase or years must be supplied")}
    timebase = (1:(years * 365 * 24)) / 24
  }
  #add mean
  temperatures   <- rep(mean, length(timebase))
  #add trend
  if (!is.null(t_int)) {
    if (is.null(t_slope)) {stop("If t_int is supplied, t_slope must also be suppled")}
    temperatures <- temperatures + ((t_slope * timebase) + t_intercept)
  }
  #loop through adding frequencies.
  for (i in 1:dim(spectrum)[1]) {     #Note: looped to save memory
    temperatures <- temperatures + (spectrum$cyc_range[i] *
                                      cos(2 * pi * spectrum$frequency[i] *
                                            (timebase - spectrum$tau[i]) +
                                            spectrum$phase[i]))
  }
  #add residuals
  if (!is.null(mean_resid)) {
    temperatures <- temperatures + rnorm(length(temperatures),
                                         mean = mean_resid,
                                         sd   = 1)
  }
  return(data.frame(jday        = timebase,
                    temperature = temperatures))
}

#' Lomb Scargle periodogram, with phase.
#'
#' Determine the periodogram of a time series using the Lomg and Scargle
#' least-square method, also return phase. This is derived from an old version
#' from the \pkg{nlts} package.
#' Warning: This is memory intensive and slow if
#' the entire spectrum is calculated, especially if it is more than 10 years,
#' and sampling is every 4 hours or more frequent.
#'
#' @param temperature A vector of temperature values.
#' @param timebase A vector of decimal Julian days. (ex: 6AM, 31s day: 31.25)
#' @param freq A vector of frequencies to test. Set to NULL for full spectrum.
#' @return A "lomb" object with temperature cycling (2 * amplitude) value,
#'   freqency, phase, p (white noise), and tau (lag).
#' @export
#' @examples
#' tropical_spectrum <- data.frame(frequency = c(1, 1 / 365),
#'                                 cyc_range = c(6.53, 4.1),
#'                                 phase     = c(0.421,0.189),
#'                                 tau       = c(0,0))
#'                                 tropical_mean <- 25.845
#' tropical_ts   <- gen_cycling_rec(years    = 2,
#'                                 spectrum = tropical_spectrum,
#'                                mean     = tropical_mean)
#'  tropical_lomb <- spec_lomb_phase(tropical$temperature,
#'                                  tropical$jday)
#'  tropical_lomb
#' @export
#'
spec_lomb_phase <- function (temperature = stop("Temperatures missing"),
                             timebase    = stop("Julian days missing"),
                             freq = c(1, 1 / 365.25)) {
  if (is.null(freq)) {
    nyear <- max(timebase) - min(timebase) + 1
    f <- seq(0, 0.5, length = nyear / 2)
  }
  else {
    f <- freq
  }
  if (length(temperature) != length(timebase))
    stop("temperature and timebase different lengths")
  if (min(f) < 0 || max(f) > 1)
    stop("freq must be between 0 and 1")
  if (min(f) == 0)
    f               <- f[f > 0]
  nt              <- length(timebase)
  nf              <- length(f)
  #from Horne and Baliunas 1986
  number.ind.freq <- -6.362 + (1.193*nt) + (0.00098*nt)^2
  ones.t          <- rep(1, nt)
  ones.f          <- rep(1, nf)
  omega           <- 2 * pi * f
  hbar            <- mean(temperature)
  hvar            <- var(temperature)
  hdev            <- temperature - hbar
  two.omega.t     <- 2 * omega %*% t(timebase)
  sum.sin         <- sin(two.omega.t) %*% ones.t
  sum.cos         <- cos(two.omega.t) %*% ones.t
  tau             <- atan(sum.sin/sum.cos) / (2 * omega)
  t.m.tau         <- (ones.f %*% t(timebase)) - (tau %*% t(ones.t))
  omega.ttau      <- (omega %*% t(ones.t)) * t.m.tau
  sin.ott         <- sin(omega.ttau)
  cos.ott         <- cos(omega.ttau)
  z               <- ((cos.ott %*% hdev)^2 / ((cos.ott^2) %*% ones.t) + (sin.ott %*% hdev)^2 / ((sin.ott^2) %*% ones.t)) / (2 * hvar)
  max             <- z == max(z,
                              na.rm = TRUE)
  max             <- max[is.na(max) == FALSE]
  #From Hocke K. 1998
  a   <- (sqrt(2/nt) * (cos.ott %*% hdev)) / (((cos.ott^2) %*% ones.t)^(1/2))
  b   <- (sqrt(2/nt) * (sin.ott %*% hdev)) / (((sin.ott^2) %*% ones.t)^(1/2))
  phi <- -atan2(b,a)
  P   <- 1 - ((1 - exp(-z[, 1]))^number.ind.freq)
  res <- list(cyc_range = sqrt(z[, 1] * 2 * hvar / (nt / 2)),
              freq      = f,
              f.max     = f[max],
              per.max   = 1 / f[max],
              phase     = phi,
              p         = P,
              tau       = tau)
  class(res) <- "lomb"
  res
}