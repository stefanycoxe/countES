#' Calculates effect size measures for count regression models
#'
#' This function calculates the standardized mean difference (SMD, i.e., Cohen's d)
#' and the multiplicative effect size for a count regression model
#' @param int Intercept value (numeric)
#' @param int_se Standard error of the intercept (numeric)
#' @param slope Slope value (numeric)
#' @param slope_se Standard error of the slope (numeric)
#' @param disp Dispersion: square root of phi for overdispersed, alpha for negative binomial (numeric)
#' @param mtype Model type ("poisson" "overdispersed" "negative binomial")
#' @param reps Number of Monte Carlo replications (integer)
#' @param CI_level Confidence interval level (e.g., 95 for 95% confidence interval)
#' @param randseed Random seed to replicate results (integer)
#' @param eff_plot Plot of exponential effect (TRUE FALSE)
#' @param dist_plot Histogram of Monte Carlo replications (TRUE FALSE)
#' @keywords count poisson negativebinomial effectsize montecarlo
#' @import ggplot2
#' @export
#' @examples
#'
#' # Poisson regression
#' countES(int = 1, int_se = 0.5, slope = 0.2, slope_se = 0.1,
#' disp = 0, mtype = "poisson",
#' reps = 10000, CI_level = 95, randseed = 12345,
#' eff_plot = TRUE, dist_plot = TRUE)
#'
#' # Overdispersed Poisson regression
#' countES(int = 1, int_se = 0.5, slope = 0.2, slope_se = 0.1,
#' disp = 1.5, mtype = "overdispersed",
#' reps = 10000, CI_level = 95, randseed = 12345,
#' eff_plot = TRUE, dist_plot = TRUE)
#'
#' countES(int = 1, int_se = 0.5, slope = 0.2, slope_se = 0.1,
#' disp = 1.5, mtype = "negative binomial",
#' reps = 10000, CI_level = 95, randseed = 12345,
#' eff_plot = TRUE, dist_plot = TRUE)
#'
#' countES()

countES <- function(int, int_se, slope, slope_se, disp, mtype, reps, CI_level, randseed, eff_plot, dist_plot){

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

if (eff_plot == TRUE) {
  y <- c(mean0, mean1)
  sd <- c(sd0, sd1)
  ymin <- y - sd
  ymax <- y + sd

  poi_model <- function(x){
    exp(int + slope * x)
  }

  exp_eff_plot <- ggplot(data.frame(x=c(-3,3)), aes(x)) +
    expand_limits(y=0) +
    stat_function(fun=poi_model, geom="line", color = "blue", size = 1) +
    geom_errorbar(aes(x = c(0,1), ymin = ymin, ymax = ymax, width = 0.2)) +
    labs(x = "Predictor", y = "Outcome variable") +
    theme_classic() +
    scale_x_continuous(breaks=seq(-3,3,1)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))

  #return(exp_eff_plot)

}

if (dist_plot == TRUE) {

  hbins <- trunc(reps / 100, 0)

  simcohdf <- data.frame(simcoh)

  smd_dist_plot <- ggplot(simcohdf, aes(simcoh)) + geom_histogram(bins = hbins, col = "blue", fill = "white") +
    theme_classic() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    labs(x = "Standardized mean difference effect",
         y = "Number of replications") +
    geom_vline(xintercept = coheff, color = "red", lwd = 1) +
    geom_vline(xintercept = cohLL, color = "blue", linetype = 2) +
    geom_vline(xintercept = cohUL, color = "blue", linetype = 2) +
    theme(legend.position = "right") +
    scale_y_continuous(expand = c(0,0))

  simb1df <- data.frame(simb1 = exp(simb1))

  exp_dist_plot <- ggplot(simb1df, aes(simb1)) + geom_histogram(bins = hbins, col = "blue", fill = "white") +
    theme_classic() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    labs(x = "Exponential effect",
         y = "Number of replications") +
    geom_vline(xintercept = expeff, color = "red", lwd = 1) +
    geom_vline(xintercept = expLL, color = "blue", linetype = 2) +
    geom_vline(xintercept = expUL, color = "blue", linetype = 2) +
    theme(legend.position = "right") +
    scale_y_continuous(expand = c(0,0))

  #return(smd_dist_plot)
  #return(exp_dist_plot)
}

  return(list(exp_eff_plot, smd_dist_plot, exp_dist_plot))

}
