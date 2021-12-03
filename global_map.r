rm(list = ls())
library(sf)
library(rgdal)
library(raster)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(rgeos)
library(tibble)

EUproj <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")
USproj <- CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs ")
testproj <- CRS("+proj=aea +lon_0=-97.03125 +lat_1=24.8027719 +lat_2=62.8771189 +lat_0=43.8399454 +datum=WGS84 +units=m +no_defs")
# ---------------------------------------------------

us2 <- readOGR(dsn="data/Data map/Boundaries", layer="cb_2015_us_state_500k")
us2 <- st_transform(us2, crs=testproj)
us2 <- as(us2, "sf")

us <- ne_countries(scale = "medium", returnclass = "sf")
us <- st_transform(us, crs = testproj)

my_theme <- theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.border = element_rect( fill = NA),
                  axis.title = element_blank(),
                  legend.position = "none")



world <-  ne_countries(scale = "medium", returnclass = "sf")
eu <- st_transform(world, crs=EUproj)


#######################

# tab %>% group_by(Reference) %>% count(Location)
nRef <-
as.data.frame(t(data.frame("North America" = 10, "Europe" = 7, "World" = 1))) %>%
  rownames_to_column("Location") %>%
  cbind("lat" = c(28,49,1)) %>%
  cbind("long" = c(-102, 18, 1))

########################

pdf("data/maps.pdf")


#### Map of the world ############
ggplot(data = eu) + geom_sf()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect( fill = NA))

## Map for the continents #########
ggplot(data = world) +
  geom_sf()+
  coord_sf(xlim = c(-130, 149), ylim = c(-35, 75), expand = FALSE)+
  geom_segment(aes(y = lat, yend = (lat+2*V1), x = long, xend = long),
               size = 2,
               # arrow = arrow(length = unit(0.15, "inches")),
               color = "blue",
               data = nRef[-3,])+
  geom_text(aes(y = ((lat+2*V1)+3), x = long, label = V1),
            color = "blue",
            data = nRef[-3,])+
  theme_classic() + my_theme

## Map for Europe ##########
ggplot(data = eu) +
  geom_sf() +
  coord_sf(xlim = c(2426378, 6293974), ylim = c(1428101, 5446513), expand = FALSE) +
  theme_classic() +  my_theme

## Map for North America
ggplot(data = us) +
  geom_sf() +
  # geom_sf(data = us2) +
  coord_sf(c(-2240784, 2568901), ylim = c(-4077524, 4005105),, expand = FALSE) +
  theme_classic() +  my_theme

dev.off()









