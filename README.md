# countES

Effect Size Measures For Count Regression Models

Uses code from RcountD shiny app: [https://stefany.shinyapps.io/RcountD/](https://stefany.shinyapps.io/RcountD/)

- Calculates effect size measures for count regression models including Poisson regression and negative binomial regression

- Standardized mean difference (SMD, i.e., Cohen's d) and multiplicative effect size measures are included

- Calculates Monte Carlo confidence intervals for the effect size

- Plots of effect size and Monte Carlo simulations are optional

To install this package:

```{r, eval = FALSE}
install.packages("devtools")
library(devtools)
devtools::install_github("stefanycoxe/countES")
```
