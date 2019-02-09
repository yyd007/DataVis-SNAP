library(sf) ## Simple Features
library(tidyverse)
library(magrittr)
library(ggplot2)
library(tidycensus)


## Consider:
path.package("sf")
install.packages("sf", lib="Resources/library/sf0_7_3")
library(sf, lib.loc="Resources/library/sf0_7_3")


## Quick theme for removing extraneous chart parts with theme_get:
## https://www.rdocumentation.org/packages/ggplot2/versions/3.1.0/topics/theme_get
theme_clean_map <- function(base_size = 12) {
  require(grid)
  theme_void(base_size) %+replace%
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





## sf documentation
## https://r-spatial.github.io/sf/index.html
## sf cheatsheet https://github.com/rstudio/cheatsheets/blob/master/sf.pdf

## geom_sf
## https://ggplot2.tidyverse.org/reference/ggsf.html

## Load in data ~about~ the geography of interest:



## Read in the shapefile (file extension .shp) with st_read (st_read can also read in geoJSON, which we will use in the D3 portion of the course):

tracts <- st_read("cb_2017_11_tract_500k/cb_2017_11_tract_500k.shp")
glimpse(tracts)
plot(st_geometry(tracts))


##
class(tracts)
attributes(tracts)

## Note you can plots all the tracts, or just some via indexing:
tracts$geometry
plot(st_geometry(tracts))
plot(st_geometry(tracts)[10:30])

## Read in points data (DC Public and Charter Schools:
dc_public <- read_csv("Public_Schools.csv")
dc_charter <- rename(read_csv("Charter_Schools.csv"), GRADES="GRADES_1")

dc_schools <- bind_rows(dc_public, dc_charter)
dc_schools %<>% mutate(CHARTER = ifelse(LEA_ID == 1, "DCPS", "Charter"))

## Different geoms with different datasets:
ggplot() + 
  geom_sf(data=tracts) + 
  geom_point(data=dc_schools, aes(x=LONGITUDE, y=LATITUDE, color=CHARTER), alpha=0.5) + 
  scale_color_manual(values=c("#1696d2", "#fdbf11")) + 
  theme_clean_map()

## Notably, you use geom_point for points, not geom_sf, more:
## https://www.cultureofinsight.com/blog/2018/05/02/2018-04-08-multivariate-dot-density-maps-in-r-with-sf-ggplot2/


## Use the Census API for American Community Survey (ACS) Data:
# census_api_key("your key here", install=TRUE)
acs_vars <- load_variables(2015, "acs5")

## See an individual variable definition:
filter(acs_vars, name =="B17017_001E")

## 11 is the FIPS code for DC:
## FIPS is the Federal Information Processing Standard 
pov <- get_acs(geography = "tract"
               , state="11"
               , variables = "B17017_001E", year = 2015)

## You can merge/join on other data as you would normally, retaining the sf class:
merged_data <- left_join(tracts, pov, by="GEOID")
glimpse(merged_data)
class(merged_data)

## Plot the choropleth with layered data on top:
ggplot() + 
  geom_sf(data=merged_data, aes(fill=estimate), color = "#ffffff") + 
  scale_fill_distiller(type = "seq", palette = 6, direction=1) + 
  geom_point(data=dc_schools, aes(x=LONGITUDE, y=LATITUDE, color=CHARTER), alpha=0.9) + 
  scale_color_manual(values=c("#1696d2", "#fdbf11")) + 
  theme_clean_map()

## Create a new sf dataframe with points collection:
schools_sf <- st_as_sf(dc_schools, coords=c("LONGITUDE","LATITUDE"))

## DC ShapeFiles are Projected with MD State Plane NAD 83
## Source: https://octo.dc.gov/page/coordinate-system-standards

## We can use an European Petroleum Survey Group (EPSG) code for shorthand, which you can look up 
## http://www.spatialreference.org/ref/?search=Maryland

schools_sf <- st_as_sf(dc_schools
                       , coords=c("LONGITUDE","LATITUDE")
                       , crs=6654
                       ,agr="constant")

## Why do we need a projection for lat/lon coordinates? They are unique to a location in the world.
plot(st_geometry(tracts))
tracts_russia <- st_transform(tracts, crs= 5940)
plot(st_geometry(tracts_russia))


