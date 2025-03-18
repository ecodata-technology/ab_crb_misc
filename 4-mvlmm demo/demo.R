
# 1. Setup

pacman::p_load(tidyverse, magrittr, lme4, lmerTest, DHARMa, broom.mixed, corrplot, dotwhisker)

data(iris)


# 2. Wrangling

# Need to restructure to long with IVs nested within DVs

long_iris = iris %>%
  select(-c(Petal.Length, Petal.Width)) %>%
  mutate(
    Individual = as.factor(row_number()),
    across(where(is.numeric), ~as.numeric(scale(.x)))
  ) %>%
  pivot_longer(
    -c(Species, Individual),
    names_to = "response_name",
    values_to = "response_value"
  )


# 3. Model

m = lmer(
  response_value ~ response_name + response_name:(Species) - 1 + (response_name - 1|Individual),
  data = long_iris,
  control = lmerControl(
    optimizer = "bobyqa",
    check.nobs.vs.nlev = "ignore",
    check.nobs.vs.nRE = "ignore"
    )
  )

summary(m)


# 4. Visualise coefficients

m_tidied = tidy(m, effect="fixed") %>%
  tidyr::separate(term, into = c("response_name", "fixeff"), sep = ':', extra = "merge", remove=FALSE) %>%
  mutate(
    response_name = gsub('response_name', '', response_name),
    fixeff = if_else(is.na(fixeff), 'Intercept', fixeff)
  )
  
dwplot(m_tidied) + facet_wrap(~fixeff, scale = "free", ncol = 2)+
  geom_vline(xintercept = 0, lty = 2) +
  scale_y_discrete(labels=sort(unique(m_tidied$response_name), decreasing=T))


# 5. Look at correlations

vv = VarCorr(m)
corrplot.mixed(cov2cor(vv$Individual),upper="ellipse")


# 6. Look at residuals

res = DHARMa::simulateResiduals(m, plot = T)
