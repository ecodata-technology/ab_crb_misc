#### Setup ####
pacman::p_load(tidyverse,magrittr,arrow,sf,readxl,lubridate,tigris)

sf_use_s2(F)

#### Setup ####

ca = tigris::counties(state="CA",resolution="20m") %>%
  rename(county=NAME) %>%
  mutate(county = tolower(county)) %>%
  st_transform(crs=4326)

strs = read_sf('2-ventura cluster/strs.gpkg')

ventura = read_xlsx('1-core area map/data/HLB_20231121.xlsx') %>% # to Dec 2023
  st_as_sf(coords = c("Final Longitude","Final Latitude"), crs=4326) %>%
  select(pdr_number = PDRNumber, collected_date = DateCollected, geometry) %>%
  mutate(collected_date = as_date(collected_date)) %>%
  st_join(ca) %>%
  filter(county == 'ventura') %>%
  st_join(strs) %>%
  mutate(month = month(collected_date))


# get STR(s) and months
ventura %>% st_drop_geometry() %>% select(month,mtrs) %>% distinct()
  
# 1    10 SBM-T03N-R21W-16
# 2    10 SBM-T03N-R21W-15
# 3     9 SBM-T03N-R21W-16

# So get predictions on input data march/april to august 2023
