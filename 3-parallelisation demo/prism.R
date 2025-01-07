# Load libraries
pacman::p_load(tidyverse,foreach,doParallel,lubridate,arrow,sf,tictoc)

# Prism library requires setting a download directory and that it already exists

# Get strs
strs = read_sf('3-parallelisation demo/data/strs.gpkg') %>%
  st_transform(4326) %>%
  mutate(id=row_number())


# Calculates zonal statistics with exactextractr, which is already highly optimized thanks to a C++ backend
# https://cran.r-project.org/web/packages/exactextractr/readme/README.html


#### Define batch function ####

# The inner function used to do the extraction on each raster is defined in prism_helper.R (quirk of how foreach works)

batch_wrangle_prism = function(type, start_date, end_date, new_grid) {
  
  cal = data.frame(date= seq.Date(from=start_date,to=end_date,by='month') ) %>%
    mutate(date = as_date(paste0(year(date),'-',month(date),'-',1)))
  
  tic("Running clusters")
  
  cl = makePSOCKcluster(detectCores()/2) # 7 cores on Alex's machine
  registerDoParallel(cl)
  
  df = foreach(i = 1:nrow(cal), .combine=rbind) %dopar% {
    source('3-parallelisation demo/climate_helper.R')
    wrangle_prism(type=type, extract_date=cal$date[i], new_grid=new_grid)
  }
  
  stopCluster(cl)
  
  toc()
  
  return(df)
}


#### Execute ####

# just a small subset of the prism data to demonstrate
# the day part doesn't matter, just month and year
start = as_date('2011-Jan-01'); end = as_date('2013-Jun-01')


# Process each variable
tic()
ppt = batch_wrangle_prism(type='ppt', start_date=start, end_date=end, new_grid=strs)
toc()
