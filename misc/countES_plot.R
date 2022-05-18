#' Plots effect with plus/minus 1 SD
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
#' @inheritParams countES_values
#' countES_plot()

countES_plot <- function(int, int_se, slope, slope_se, disp, mtype, reps, CI_level, randseed){

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

  ymin <- expLL
  ymax <- expUL
  y <- expeff()
  x <- 0
  df <- data.frame(x, y, ymin, ymax)

  eff_plot <- ggplot(data = df, aes(x=x, y=y)) +
    geom_errorbar(width=.2, aes(ymin=ymin, ymax=ymax)) +
    geom_point(x = x, y = y) +
    theme_classic() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    expand_limits(y=c(0, NA)) +
    xlim(-2, 2) + geom_hline(aes(yintercept = 1), color = "blue", size = 2) +
    labs(x = "", y = "CI for exponential effect size", title =paste(round(expeff(), digits = 3),"times larger"))


}
