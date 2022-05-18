#' Plots histogram of Monte Carlo simulation values with confidence interval
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
#' countES_dist()

countES_dist <- function(int, int_se, slope, slope_se, disp, mtype, reps, CI_level, randseed){

  smd_dist_plot <- ggplot(simcohdf, aes(simcoh)) + geom_histogram(bins = hbins(), col = "blue", fill = "white") +
    theme_classic() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    labs(x = "Standardized mean difference effect",
         y = "Number of replications") +
    geom_vline(xintercept = coheff(), color = "red", lwd = 1) +
    geom_vline(xintercept = cohLL(), color = "blue", linetype = 2) +
    geom_vline(xintercept = cohUL(), color = "blue", linetype = 2) +
    theme(legend.position = "right") +
    scale_y_continuous(expand = c(0,0))

  exp_dist_plot <- ggplot(simb1df, aes(simb1)) + geom_histogram(bins = hbins(), col = "blue", fill = "white") +
    theme_classic() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    labs(x = "Exponential effect",
         y = "Number of replications") +
    geom_vline(xintercept = expeff(), color = "red", lwd = 1) +
    geom_vline(xintercept = expLL(), color = "blue", linetype = 2) +
    geom_vline(xintercept = expUL(), color = "blue", linetype = 2) +
    theme(legend.position = "right") +
    scale_y_continuous(expand = c(0,0))
}

