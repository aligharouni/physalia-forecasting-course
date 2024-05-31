library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2); theme_set(theme_bw())
library(mgcv)
library(marginaleffects)
library(dplyr)
library(gratia)
library(mvgam)


# update any installed R packages
update.packages(ask = FALSE, checkBuilt = TRUE)

# packages to install for the course
pkgs <- c("brms", "dplyr", "gratia", "ggplot2", "marginaleffects",
          "tidybayes", "zoo", "viridis", "mvgam")

# install packages
install.packages(pkgs)


# library(mvgam)
# simdat <- sim_mvgam()
# mod <- mvgam(y ~ s(season, bs = 'cc', k = 5) +
#                s(time, series, bs = 'fs', k = 8),
#              data = simdat$data_train)

# Read in the mosquito data
mos_dat <- read_xlsx('mosquitos.xlsx') %>%
  dplyr::mutate(series = as.factor(Lugar),
                month = Mes) %>%
  dplyr::group_by(series) %>%
  dplyr::arrange(year, month) %>%
  dplyr::ungroup()

# Ensure all series have an observation for all possible timepoints;
# unfortunately this will result in missing values for covariates as well,
# so mvgam may not be an option here
all_times <- expand.grid(year = seq(min(mos_dat$year),
                                    max(mos_dat$year)),
                         month = 1:12,
                         series = unique(mos_dat$series))

mos_dat %>%
  dplyr::select(-Time) %>%
  dplyr::right_join(all_times) %>%
  dplyr::group_by(series) %>%
  dplyr::arrange(year, month) %>%
  dplyr::mutate(time = dplyr::row_number()) %>%
  dplyr::ungroup() -> mos_dat

# Number of missing values for each region
mos_dat %>%
  dplyr::filter(is.na(Nhembras)) %>%
  dplyr::group_by(series) %>%
  dplyr::tally()

dat$y[dat$series == 'Canada' & dat$month == 12] <- NA

glimpse(mos_dat)


# Plot the region-level time series
ggplot(mos_dat, aes(time, Nhembras, col = series)) +
  geom_point() +
  facet_wrap(~series, scales = 'free_y') +
  scale_y_log10() + 
  theme(legend.position = 'none')

# Look at region-level histograms
ggplot(mos_dat, aes(Nhembras, fill = series)) +
  geom_histogram(col = 'white') +
  facet_wrap(~series, scales = 'free_x') +
  theme(legend.position = 'none')


(mos_dat
  %>% ggplot(aes(x=time, y=Nhembras, group = series))
  + geom_line(aes(color=series))
  + scale_y_log10() 
  )

mod1 <-bam(Nhembras ~ 
             series+
               te(Tmean, month, k = c(10, 10)) +
               te(Tmean, month, k = c(10, 10), by = series),
             family = tw(), 
             data = mos_dat,
           discrete = TRUE
             # newdata = mos_dat[951:960, ],
             # trend_model = 'None'
           )

# ? mgcv::Tweedie

summary(mod1)
gam.check(mod1)

plot_predictions(model = mod1, by = c("Tmean","month","series"))
#> Notes: what these plots are showing is what we expect 
#> by= plot based on all obs data
#> condition=
#> 
#> 
#> 

mod2 <- bam(Nhembras ~ series+WindVel+
              # s(series, bs = 're') + 
              s(time, k = 20, bs = "tp", by = series) + ## s(year) ?
              # s(floodedHa5km, k = 20, bs = "tp", by = series) +
              s(PrecAc, k = 20, bs = "tp", by = series) +
              s(Tmean, k = 10, bs = "cr", by = series) ,
              # s(WindVel, k=5,bs = "cr", by = series),
            data = mos_dat, 
            AR.start = time==1, 
            rho = .8,
            family=tw(),
            discrete = TRUE)

summary(mod2)

## Another model is distributed lag, marginal effect with Autocorrelations,

gratia::draw(mod2)

# plot_predictions(model = mod2, condition = c("time", "floodedHa5km","Tmean","series"), type = "link")
plot_predictions(model = mod2, condition = c("PrecAc","Tmean","series"), type = "link")
# plot_predictions(model = mod2, condition = c("Tmean","time","series"), type = "link")
# mod1 <-mvgam(Nhembras ~ 
#                te(Tmean, month, k = c(4, 4)) +
#                te(Tmean, month, k = c(4, 4), by = series),
#              family = gaussian(),
#              data = mos_dat[1:950,],
#              newdata = mos_dat[951:960, ],
#              trend_model = 'None')



# Some very big differences in counts here, hopefully some of the covariates
# can help explain these 