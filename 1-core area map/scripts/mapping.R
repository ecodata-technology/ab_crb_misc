# cities obtained from: https://gis.data.ca.gov/datasets/CALFIRE-Forestry::california-incorporated-cities-1/about
# last updated May 2024


#### Setup ####
pacman::p_load(tidyverse,magrittr,arrow,sf,readxl,lubridate,tigris,showtext,sysfonts)


# Visual

# Plot themes
source("./Assets/theme_ecodata.R")
logo="<img src='./Assets/footer_2024.png' height='11' />"


# colours
bg = "#1b2724"
wh = "#ffffff"
pl = "#6eb39c"

bla = "#212932"
wh = "#ffffff"
blu = "#167799"
lblu = "#008f96"
or = "#feab00"
dor = "#fd7d00"
gr = "#98c91a"
dgr = "#5ea744"

# fonts
sysfonts::font_add_google("Lexend Deca", "LexendDecaSemiBold", regular.wt = 600)
sysfonts::font_add_google("Lexend Deca", "LexendDecaThin", regular.wt = 200)
sysfonts::font_add_google("Lexend Deca", "LexendDecaLight", regular.wt = 400)
sysfonts::font_add_google("Roboto Condensed", "RobotoCondensed")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)



# Get geometries
focal_counties = c("orange","los angeles","riverside","san bernardino")

ca = tigris::counties(state="CA",resolution="20m") %>%
  rename(county=NAME) %>%
  mutate(county = tolower(county))

cazc = tigris::zctas()

zc_counties = cazc %>%
  st_join(ca) %>%
  filter(STATEFP == '06') %>%
  select(zip = ZCTA5CE20, county, geometry) %>%
  st_transform(crs=4326)


#### Data wrangling ####


# Get detections and compile new data

dat_1 = read_parquet('1-core area map/data/plant_ct.parquet') %>% # to Sep 2023
  st_as_sf(coords = c("longitude","latitude"), crs=4326) %>%
  filter(results %in% c("POSITIVE")) %>%
  select(pdr_number,collected_date,geometry)
dat_2 = read_xlsx('1-core area map/data/HLB_20231121.xlsx') %>% # to Dec 2023
  st_as_sf(coords = c("Final Longitude","Final Latitude"), crs=4326) %>%
  select(pdr_number = PDRNumber, collected_date = DateCollected, geometry) %>%
  mutate(collected_date = as_date(collected_date))

# Aggregate to PDRs
dat = dat_1 %>%
  bind_rows(
    dat_2 %>% filter(collected_date > max(dat_1$collected_date))
  ) %>%
  mutate(
    year = year(collected_date),
    month = month(collected_date)
  ) %>%
  group_by(pdr_number) %>%
  summarise(n_pos = n()) %>%
  ungroup()
  

# Spatial join and aggregate to zips


dat_zc = zc_counties %>%
  st_join(dat) %>%
  filter(!is.na(n_pos)) %>%
  group_by(zip, county) %>%
  summarise(total_pos = n()) %>%
  ungroup()

# Get quantiles
dat_q = quantile(dat_zc$total_pos, probs=seq(0,1,0.05)) %>%
  as.data.frame() %>%
  rename(threshold = 1) %>%
  rownames_to_column(var="quantile") %>%
  mutate(quantile = as.numeric(gsub("%","",quantile)))
  



#### Mapping ####

# Plot percentiles
ggplot(dat_q, aes(x=threshold,y=quantile)) +
  geom_line() +
  labs(x="HLB+ Detections", y="Quantile") +
  scale_y_continuous(breaks = seq(0,100,10), labels= ~ paste0(.x,'th')) +
  theme_bw()

# Set threshold at 95th percentile
th = dat_q$threshold[dat_q$quantile==95]

# Classify
dat_zc %<>% mutate(core = if_else(total_pos >= th,"High","Low"))

# zoomed out
ggplot() +
  geom_sf(data=dat_zc, aes(fill=core,colour=core)) +
  geom_sf(data={ca %>% filter(county %in% focal_counties)}, colour="black", fill=NA, linewidth=0.5)



# zoomed in
p = ggplot() +
  geom_sf(data={ca %>% filter(county %in% focal_counties)}, colour="black", fill="grey", linewidth=1) +
  geom_sf(data=dat_zc, aes(fill=core),colour="black",linewidth=0.1) +
  #labs(caption=paste0("<br><br>",logo)) +
  scale_fill_manual(values=c(or,"lightblue")) +
  scale_x_continuous(limits=c(-118.4,-117.2)) +
  scale_y_continuous(limits=c(33.6,34.4)) +
  theme_ecodata_noggtext(base_size=8) +
  theme(
    plot.background = element_rect(fill="white",colour="white"),
    #panel.background = element_rect(fill="grey",colour="grey"),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    legend.text = element_text(size=8),
    legend.justification="top",
    legend.title = element_blank(),
    plot.subtitle = element_text(lineheight=.4)
  )

ggsave(p, filename="zip_95th_percentile.png",path="./1-core area map/outputs/",
       width=18,height=12,units="cm",dpi=300)
