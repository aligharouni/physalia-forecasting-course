#> Intro to mgcv is the goal here.
#> 


library(mgcv)
library(marginaleffects) ## for plot_predictions()
library(dplyr)
library(gratia)
library(ggplot2); theme_set(theme_bw())

# Load the annual American kestrel abundance time series in 
# taken in British Columbia, Canada. These data have been collected 
# annually, corrected for changes in observer coverage and detectability,
# and logged. They can be found in the MARSS package
load(url('https://github.com/atsa-es/MARSS/raw/master/data/kestrel.rda'))
View(kestrel)

# Arrange the data into a data.frame
regions <- c("BC", "Alb", "Sask")
model_data <- do.call(rbind, lapply(seq_along(regions), function(x){
  data.frame(year = kestrel[,1],
             logcount = kestrel[,1 + x],
             region = regions[x])
}))
View(model_data)

# Factors for year and region
model_data %>%
  dplyr::mutate(yearfac = as.factor(year),
                region = as.factor(region)) -> model_data

# Inspect the data structure and dimensions
dplyr::glimpse(model_data)
levels(model_data$yearfac)
levels(model_data$region)

# Plot the time series
ggplot(model_data, aes(x = year, y = logcount)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~region)

# The adjusted logcount never goes below zero
ggplot(model_data, aes(logcount)) +
  geom_histogram(col = 'white')
summary(model_data$logcount)

# Q. What kind of observation family is appropriate?
?family
?mgcv::family.mgcv

# Maybe a Gamma?
# Random effects in mgcv use the 're' basis
# AG: grouping can be handled in mgcv too
?mgcv::smooth.construct.re.smooth.spec

# A model with yearly random intercepts
mod <- gam(logcount ~ s(yearfac, bs = 're'),
           data = model_data,
           family = Gamma(link = 'inverse'),
           method = 'REML')

# Oops; turns but it does contain actual zeros
any(model_data$logcount == 0L)

# A Tweedie model may be best here
mod <- gam(logcount ~ s(yearfac, bs = 're'),
           data = model_data,
           family = Tweedie(p = 1.5), ## this p is handle the overdispersion and 1.5 is a good start
           method = 'REML')

# Random effect variance component
gam.vcomp(mod)

# Summary
summary(mod)

# Residual diagnostics (using gratia)
appraise(mod)
#> the topright panel is res against predictor

# Looks poor! Why? Look at the random effect estimates
?marginaleffects::plot_predictions
plot_predictions(mod, condition = 'yearfac') ## what is the effect of year factor ?

# Look at residuals against region?
resids <- residuals(mod)
ggplot(data.frame(model_data %>%
                    dplyr::bind_cols(data.frame(resids))),
       aes(x = year, y = resids)) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~region)

# Aha! Need to add region as a factor as well
mod2 <- gam(logcount ~ s(yearfac, bs = 're') + 
              s(region, bs = 're'), ## the trend stays the same but the ave effect on mean would be different 
            data = model_data,
            family = Tweedie(p = 1.5),
            method = 'REML')
gam.vcomp(mod2)
summary(mod2)
appraise(mod2)

# Look at residuals again
resids <- residuals(mod2)
ggplot(data.frame(model_data %>%
                    dplyr::bind_cols(data.frame(resids))),
       aes(x = year, y = resids)) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~region)

# Better, but still obvious problems 
# (the trend is not the same per region, how can we model this?)

plot_predictions(mod2, by=c('yearfac','region'))
## 
## by : plot predictions for each year and region, but the ave counts differ
## This shows that the ave counts (response var) different across regions but the trend stays the same (as we modeled it that way)

## the 3rd thing in by is the facets
plot_predictions(mod2, by=c('yearfac','region','region'))
## 

#### Bonus tasks ####
# 1. Calculate residual ACF and pACF functions for each region using
#    the residuals from mod2. Hint, you can add residuals to the 
#    data object as we've done above

# 2. Look at the help file on random effects in mgcv and work out
#    how to fit a model that includes random slopes of year. It'll be
#    best to scale the year variable beforehand for easier computation
model_data %>%
  dplyr::mutate(year_scaled = as.vector(scale(year))) -> model_data

# 3. Look at residuals against region for this model, and use
#    plot_predictions() to show the predicted linear year functions
#    for each region

# 4. Compare the model with random slopes against mod2 using a 
#    Generalized Likelihood Ratio test (hint, see anova.gam() for help)
?anova.gam
