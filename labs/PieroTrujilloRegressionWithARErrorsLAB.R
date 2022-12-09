# Piero Trujillo
# Regression With AR Errors LAB

library(tidyverse)
library(lubridate)
library(forecast)
library(fda)

url <- 'https://raw.githubusercontent.com/pstat197/pstat197a/main/materials/labs/lab8-forecasting/data/soiltemp-200cm.csv'

soil <- read_csv(url) %>%
  dplyr::select(-year, -elev) %>%
  filter(!str_starts(site, 'SHA'))

# choose a site at random
set.seed(111522) # comment out!
nsites <- soil %>% pull(site) %>% unique() %>% length()
site_ix <- sample(1:nsites, size = 1)

# filter rows
site_data <- soil %>% 
  filter(site == unique(soil$site)[site_ix])

# preview
site_data %>% head()

# predictor matrix
xreg <- site_data %>%
  pull(day) %>%
  fda::fourier(nbasis = 4, period = 365)

# response
y <- pull(site_data, temp)

# create a data frame
reg_df <- bind_cols(temp = y, 
                    xreg)

# fit the model
fit <- lm(temp ~ . - 1, data = reg_df)

# obtain fitted values, residuals, etc.
fit_df <- broom::augment(fit) %>%
  bind_cols(date = site_data$date)

# plot residual series
fit_df %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_path()

# plot residuals at various lags
fit_df %>%
  dplyr::select(.resid) %>%
  mutate(lag1 = lag(.resid, n = 1),
         lag2 = lag(.resid, n = 2),
         lag3 = lag(.resid, n = 3),
         lag4 = lag(.resid, n = 4),
         lag5 = lag(.resid, n = 5),
         lag6 = lag(.resid, n = 6)) %>%
  pivot_longer(-.resid) %>%
  ggplot(aes(x = .resid, y = value)) +
  geom_point() + 
  facet_wrap(~ name)

resid_acf <- acf(fit_df$.resid, plot = F)

plot(resid_acf, main = '')

resid_pacf <- pacf(fit_df$.resid, plot = F)

plot(resid_pacf, main = '')

# fit error model
fit_resid <- Arima(fit_df$.resid,
                   order = c(2, 0, 0),
                   include.mean = F,
                   method = 'ML')

resid_acf_fitted <- ARMAacf(ar = coef(fit_resid), 
                            lag.max = 25)

plot(resid_acf, main = '')
lines(resid_acf_fitted, col = 'red')

predict(fit_resid, n.ahead = 5)

forecast(fit_resid, h = 5)

# determine a point at which to cut the series
cutpt <- nrow(xreg) - 30

# training series
y_train <- y[1:cutpt]
x_train <- xreg[1:cutpt, ]

# fit the model
fit_full <- Arima(y_train,
                  order = c(2, 0, 0),
                  xreg = x_train,
                  include.mean = F,
                  method = 'ML')

broom::tidy(fit_full) %>% knitr::kable()

# testing series
y_test <- y[(cutpt + 1):nrow(xreg)]
x_test <- xreg[(cutpt + 1):nrow(xreg), ]

preds <- forecast(fit_full, 
                  h = nrow(x_test), 
                  xreg = x_test)

preds %>% as_tibble() %>% head()

fig_forecast <- site_data %>%
  dplyr::select(date, temp) %>%
  bind_cols(pred = c(fit_full$fitted, preds$mean),
            status = c(rep('obs', nrow(x_train)),
                       rep('pred', nrow(x_test)))) %>%
  filter(date >= ymd('2018-01-01')) %>% # adjust if needed
  ggplot(aes(x = date)) +
  geom_path(aes(y = temp, linetype = status)) +
  geom_path(aes(y = pred), color = 'blue', alpha = 0.5)

fig_forecast

ci_df <- site_data %>% 
  slice_tail(n = nrow(x_test)) %>% 
  dplyr::select(date) %>%
  bind_cols(lwr = preds$lower[, 2],
            upr = preds$upper[, 2])

fig_forecast +
  geom_ribbon(aes(x = date, ymin = lwr, ymax = upr),
              alpha = 0.3, fill = 'blue',
              data = ci_df)