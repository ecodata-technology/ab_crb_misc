
# setup
pacman::p_load(tidyverse, tidyr, furrr, rlang, glue, zoo)

set.seed(42)

# generate some data
dat = data.frame(
  month = c(1:24) + 6, # just to illustrate a point
  precip = sample(0:20, 24, replace=T),
  temp = sample(10:35, 24, replace=T),
  humidity = sample(0:100, 24, replace=T)
  ) %>%
  mutate(row = row_number())


# generate grid of features
features = tidyr::expand_grid(
  lookback = c(1:6),
  feature = c('precip', 'temp', 'humidity')
)

# set # cores
future::plan(multisession, workers = 4)


# calculate features in parallel
rolling_averages = features %>%
  mutate(
    result = furrr::future_map2(feature, lookback, 
                                ~ zoo::rollmeanr(pull(dat,.x), .y)
                                )
  ) %>%
  unnest(result) %>%
  group_by(feature,lookback) %>%
  mutate(row = row_number()) %>%
  left_join(dat %>% select(month, row)) %>%
  ungroup()


# pivot wider for a standard tidy wide format
ra_wide = rolling_averages %>%
  pivot_wider(
    names_from = c('feature','lookback'),
    names_glue = '{feature}_{lookback}_month_ma',
    values_from = result
  )
