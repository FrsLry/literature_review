# rm(list = ls())
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
as.data.frame(t(data.frame("North America" = nrow(tab[grepl("USA", tab$Location),]),
                           "Europe" = nrow(tab[!grepl("USA", tab$Location) & !grepl("World", tab$Location),]),
                           "World" = nrow(tab[grepl("World", tab$Location),])))) %>%
  rownames_to_column("Location") %>%
  cbind("lat" = c(28,49,1)) %>%
  cbind("long" = c(-102, 18, 1))

########################

pdf("data/maps.pdf")


#### Map of the world ############
ggplot() + geom_sf(data = eu)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect( fill = NA))


test <-
ggplot()+
        geom_text(aes(0, 0),
                  label = tab %>% filter(`Spatial grain (Km²)` == "Global") %>%
                    group_by(Metric) %>% count() %>% unite(Metric, c("n", "Metric"), sep = " ") %>%
                    pull(Metric) %>% str_flatten(., collapse = "\n"),
                  hjust = 0,
                  family = "sans")+
        theme_void()
print(test)


## Map for the continents #########
ggplot(data = world) +
  geom_sf()+
  coord_sf(xlim = c(-130, 149), ylim = c(-35, 75), expand = FALSE)+
  geom_segment(aes(y = lat, yend = (lat+(1/2*V1)), x = long, xend = long),
               size = 2,
               # arrow = arrow(length = unit(0.15, "inches")),
               color = "red",
               data = nRef[-3,])+
  geom_text(aes(y = ((lat+(1/2*V1))+5), x = long, label = V1),
            color = "red",
            data = nRef[-3,],
            fontface = "bold", size = 5)+
  theme_classic() + my_theme

## COunnts
ggplot()+
  geom_text(aes(0,0),
            label = tab[grepl("USA", tab$Location),] %>%
              group_by(Metric) %>% count() %>% unite(Metric, c("n", "Metric"), sep = " ") %>%
              pull(Metric) %>% str_flatten(., collapse = "\n"),
            hjust= 0,
            family = "sans")+
  ggtitle("North America")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot()+
  geom_text(aes(0,0),
            label = tab[!grepl("USA", tab$Location) & !grepl("World", tab$Location),] %>%
              group_by(Metric) %>% count() %>% unite(Metric, c("n", "Metric"), sep = " ") %>%
              pull(Metric) %>% str_flatten(., collapse = "\n"),
            hjust= 0,
            family = "sans")+
  ggtitle("Europe")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))





## Map for Europe ##########
ggplot(data = eu) +
  geom_sf() +
  coord_sf(xlim = c(2426378, 6293974), ylim = c(1428101, 5446513), expand = FALSE) +
  theme_classic() +  my_theme

plot.new()
gridExtra::grid.table(tab[!grepl("USA", tab$Location) & !grepl("World", tab$Location),] %>%
        group_by(Location) %>% count(Metric))


## Map for North America #####
ggplot(data = us) +
  geom_sf() +
  # geom_sf(data = us2) +
  coord_sf(c(-2240784, 2568901), ylim = c(-2177524, 1505105), expand = FALSE) +
  theme_classic() +  my_theme

plot.new()
gridExtra::grid.table(tab[grepl("USA", tab$Location),] %>%
                        group_by(Location) %>% count(Metric))

dev.off()









