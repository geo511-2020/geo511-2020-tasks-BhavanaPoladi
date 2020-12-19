library(raster)
library(sp)
library(spData)
library(tidyverse)
library(sf)
library(dplyr)

##### prepare country polygon ########
data(world)
world_without_Antarctica <- world %>%
  filter(continent != "Antarctica")
filtered_world_sp <- as(world_without_Antarctica, "Spatial") 

###### prepare climate data ###########
tmax_monthly <- getData(name="worldclim", var="tmax", res=10)
tmax_monthly
plot(tmax_monthly)

tmax_annual <- max(tmax_monthly)
names(tmax_annual) <- "tmax"
gain(tmax_annual) <- 0.1

###### calculate max temp in each country #######
country_tmax <- raster::extract(tmax_annual, filtered_world_sp, 
                                fun=max, na.rm=T, small=T, sp=T)
country_tmax_sf <- st_as_sf(country_tmax)

###### Results #######
plot1 <- ggplot(data=country_tmax_sf, aes(fill=tmax)) +
  geom_sf() +
  scale_fill_viridis_c(name="Annual\nMaximum\nTemperature (C)") +
  theme(legend.position='bottom')
plot1

####### finding hottest country #####
hottest_country <- country_tmax_sf %>%
  select(name_long, continent, tmax) %>%
  arrange(desc(tmax)) %>%
  st_set_geometry(NULL) %>%
  group_by(continent) %>%
  top_n(1, tmax)       ###Sandra suggested to use tmap here####
  hottest_country

#ggsave("plot1_casestudy6.png")