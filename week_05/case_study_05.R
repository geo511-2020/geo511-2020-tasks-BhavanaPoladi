library(spData)
library(sf)
library(tidyverse)
library(units)

data(world)  
data(us_states)

albers = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
world_transform <- st_transform(world, crs = albers)

filt_Canada <- world_transform %>%                  
  filter(name_long == "Canada")                #learned about transform from Betsy

buffer_Canada <- st_buffer(filt_Canada, dist = 10000)   

US_transform <- st_transform(us_states, albers)
head(us_states)

filt_NY <- US_transform %>%
  filter(NAME == "New York")
view(filt_NY)
border <- st_intersection(st_geometry(filt_NY), st_geometry(buffer_Canada))

final <- ggplot(data = filt_NY) + geom_sf() + 
  geom_sf(data = border, fill = "red") + 
  labs(title = "New York Land within 10km") + 
  theme(plot.title = element_text(size = 22)) + 
  theme(axis.text = element_text(size = 12))
print(final)

border_area <- st_area(border) %>%
  set_units(km^2)
print(border_area)
ggsave(filename = "finalmap_casestudy5.png", device = "png")