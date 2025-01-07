# Function to extract summary stats from a raster based on a new grid (polygon data)
pacman::p_load(tidyverse,glue,lubridate,prism,sf,raster,terra,exactextractr,rlang)


wrangle_prism = function(type,extract_date,new_grid) {
  
  # Open the subdir
  dir = glue('3-parallelisation demo/data/{type}')
  prism_set_dl_dir(dir)
  
  # Fetch the raster
  temp_raster = prism_archive_subset(type=type, temp_period='monthly', years=year(extract_date), mon=month(extract_date)) %>%
    pd_stack()
  
  # Realign to STRs
  # Warning message is a known issue in the github, it's ambiguous but the correct projection is indeed being used
  extract = exact_extract(
    temp_raster,
    new_grid,
    'mean'
  )
  
  collate = new_grid %>%
    st_drop_geometry(geom) %>%
    cbind(extract) %>%
    mutate(date = extract_date) %>%
    dplyr::select(mtrs, date, !!type := extract)
  
  return(collate)
  
}