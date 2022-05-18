#' Calculates effect size with confidence interval
#'
#' This function calculates the standardized mean difference (SMD, i.e., Cohen's d)
#' and the multiplicative effect size for a count regression model
#' @param int Intercept value
#' @param int_se Standard error of the intercept
#' @param slope Slope value
#' @param slope_se Standard error of the slope
#' @param disp Dispersion
#' @param mtype Model type: "poisson" "overdispersed" "negative binomial"
#' @param reps Number of Monte Carlo replications
#' @param CI_level Confidence interval level (e.g., 95)
#' @param randseed Random seed to replicate results
#' @keywords count poisson negativebinomial effectsize montecarlo
#' @export
#' @examples
#'
#' # Poisson regression
#' count_ES(int = 1, int_se = 0.5, slope = 2, slope_se = 1,
#' disp = 0, mtype = "poisson",
#' reps = 10000, CI_level = 95, randseed = 12345)
#'
#' # Overdispersed Poisson regression
#' count_ES(int = 1, int_se = 0.5, slope = 2, slope_se = 1,
#' disp = 1.5, mtype = "overdispersed",
#' reps = 10000, CI_level = 95, randseed = 12345)
#'
#' count_ES(int = 1, int_se = 0.5, slope = 2, slope_se = 1,
#' disp = 1.5, mtype = "negative binomial",
#' reps = 10000, CI_level = 95, randseed = 12345)
#'
#' countES_values()

countES_values <- function(int, int_se, slope, slope_se, disp, mtype, reps, CI_level, randseed){

  set.seed(randseed)

  mean0 <- exp(int)

  mean1 <- exp(int)*exp(slope)

  sd0 <- if (mtype == "poisson") {
    sd0 <- sqrt(mean0)
  } else if (mtype == "overdispersed") {
    sd0 <- sqrt(disp * mean0)
  } else if (mtype == "negative binomial") {
    sd0 <- sqrt(mean0 + (disp * mean0 * mean0))
  }
  sd0

  sd1 <- if (mtype == "poisson") {
    sd1 <- sqrt(mean1)
  } else if (mtype == "overdispersed") {
    sd1 <- sqrt(disp * mean1)
  } else if (mtype == "negative binomial") {
    sd1 <- sqrt(mean1 + (disp * mean1 * mean1))
  }
  sd1

  expeff <- exp(slope)

  coheff <- (mean1 - mean0)/sd0

  simb0 <- rnorm(reps) * int_se + int

  simb1 <- rnorm(reps) * slope_se + slope

  simmean0 <- exp(simb0)

  simmean1 <- exp(simb0) * exp(simb1)

  simsd0 <- if (mtype == "poisson") {
    simsd0 <- sqrt(simmean0)
  } else if (mtype == "overdispersed") {
    simsd0 <- sqrt(disp * simmean0)
  } else if (mtype == "negative binomial") {
    simsd0 <- sqrt(simmean0 + (disp * simmean0 *simmean0))
  }
  simsd0

  simcoh <- (simmean1 - simmean0) / simsd0

  CI_lo <- (1 - CI_level/100)/2

  CI_hi <- ((1 - CI_level/100)/2) + (CI_level/100)

  cohLL <- quantile(simcoh, CI_lo)

  cohUL <- quantile(simcoh, CI_hi)

  expLL <- quantile(exp(simb1), CI_lo)

  expUL <- quantile(exp(simb1), CI_hi)

  #print(expeff)
  #print(coheff)
  exp_val <- cbind(expeff, expLL, expUL)
  coh_val <- cbind(coheff, cohLL, cohUL)
  ef_dat <- rbind(exp_val, coh_val)
  rownames(ef_dat) <- c("Exponential", "SMD")
  colnames(ef_dat) <- c("Effect size", "Lower limit", "Upper limit")
  print(ef_dat)

}
