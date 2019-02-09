library(sf) ## Simple Features
library(tidyverse)
library(ggplot2)


## sf documentation
## https://r-spatial.github.io/sf/index.html
## sf cheatsheet https://github.com/rstudio/cheatsheets/blob/master/sf.pdf

## geom_sf
## https://ggplot2.tidyverse.org/reference/ggsf.html

## Load in data ~about~ the geography of interest:
setwd("~/Desktop/MACSS/5th Quarter/Data Visualization/SNAP/SNAP Project/dc_wards_sf_ggplot2")
wards <- read.csv("wards.csv")
glimpse(wards)


## Read in the shapefile (file extension .shp) with st_read (st_read can also read in geoJSON, which we will use in the D3 portion of the course):

ward_shp <- st_read("WardPly/WardPly.shp")
glimpse(ward_shp)

## Shapefiles are really a collection of 3-7 files:
# .shp is the main file with shape information (point/line/polygon); 
# .shx is an index file 
# .dbf contains features, one observation per row
# .prj is projection information


## Glimpse stills works to look at the data stored within a dataframe in ward_shp, but if we examine the class, we see ward_shp is also a class of 'sf', meaning it has additional data and structure.

class(ward_shp)
attributes(ward_shp)

## Most notable is the sf_column, called `geometry`
ward_shp$geometry

## This allows for interesting defacult behavior if we pass it into plot:
plot(ward_shp)

## More usefully, using st_geomtry, which retrieves the special `sf_column` from our simple features dataframe: 
plot(st_geometry(ward_shp))
?st_geometry


## Look at one value for the sf_column, geometry:
print(ward_shp$geometry[[1]])


## You can merge/join on other data as you would normally, retaining the sf class:
merged_data <- left_join(ward_shp, wards, by = c("WARD"="Ward"))
glimpse(merged_data)
class(merged_data)


ggplot() + geom_sf(data = merged_data) 

ggplot() + geom_sf(data = merged_data, aes(fill=pop), color="#cccccc") + 
  labs(title="DC Population By Ward", fill="Population")

nc_map <- get_map(location = "Washington, DC", zoom = 7)

st_centroid(ward_shp)



ggplot() + geom_sf(data=merged_data, aes(fill=pop), color="#cccccc") + 
  scale_fill_distiller(palette = "Spectral", 
                       breaks=(c(70000,75000,80000)),
                       labels=(c(70000,75000,80000))) + 
  geom_sf_label(data=merged_data, aes(label=LABEL)) + 
  labs(title="DC Population By Ward", fill="Population")

## Available geoms/stats/coords are here:
## https://ggplot2.tidyverse.org/reference/ggsf.html
## geom_sf; stat_sf; geom_sf_label; geom_sf_text; coord_sf


## Quick theme for removing extraneous chart parts with theme_get:
## https://www.rdocumentation.org/packages/ggplot2/versions/3.1.0/topics/theme_get
theme_clean_map <- function(base_size = 12) {
  require(grid)
  theme_grey(base_size) %+replace%
    theme(
      axis.title      =   element_blank(),
      axis.text       =   element_blank(),
      axis.ticks       =   element_blank(),
      panel.background    =   element_blank(),
      panel.grid      =   element_blank(),
      panel.grid.major =  element_line(size = 0),
      panel.grid.minor = element_blank(),
      panel.spacing    =   unit(0,"lines"),
      plot.margin     =   unit(c(0,0,0,0),"lines"),
      complete = TRUE
    )
}

## Or you can use theme set, though this has some disadvantages:
theme_set(
  theme_minimal() +
    theme(panel.grid.major = element_line(size = 0),
          plot.background = element_rect(fill = "#f1f1ed", colour = NA),
          axis.title = element_blank(),
          text = element_text(family = "Arial"),
          axis.text = element_blank(),
          legend.position = "bottom"
  )
)

ggplot() + geom_sf(data=merged_data, aes(fill=pop), color="#cccccc") + 
  scale_fill_distiller(palette = "Spectral", 
                       breaks=(c(70000,75000,80000)),
                       labels=(c(70000,75000,80000))) + 
  geom_sf_label(data=merged_data, aes(label=LABEL)) + 
  labs(title="DC Population By Ward", fill="Population")


## Gather the data from wide to long:
long_data <- gather(merged_data, key=vars, value=value, pop, AffHousing) 

## Facet to makee multiple maps
ggplot() + geom_sf(data = long_data, aes(fill=value), color="#cccccc") + 
  facet_wrap(~vars) 

## More:
# Dot Density Maps: https://tarakc02.github.io/dot-density/ 
# Multivariate dot density maps: https://www.cultureofinsight.com/blog/2018/05/02/2018-04-08-multivariate-dot-density-maps-in-r-with-sf-ggplot2/
# Beautiful Maps: https://www.r-spatial.org//r/2018/10/25/ggplot2-sf.html
