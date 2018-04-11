# create map of poverty by county

## Dependencies
### tidyverse
library(dplyr)
library(ggplot2)

### mapping
library(classInt)
library(sf)
library(tidycensus)
library(tigris)

### other
library(prener)

## Create state outline
### download missouri
mo <- states(cb = FALSE, resolution = "20m")

### convert to sf object
mo <- st_as_sf(mo)

### subset observations
mo <- filter(mo, STATEFP == 29)

## Create combined census tract object
### download missouri counties
moCounties <- counties(state = "MO", cb = FALSE, resolution = "20m")

### convert to sf object
moCounties <- st_as_sf(moCounties)

### subset columns
moCounties <- select(moCounties, GEOID, COUNTYFP, NAMELSAD, ALAND)

### download county population data
moPoverty <- get_acs(geography = "county",  state = "MO", output = "wide", table = "B17001")

### subset columns and calculate estimate
moPoverty %>%
  mutate(pctPoverty = (B17001_002E/B17001_001E)*100) %>%
  select(GEOID, B17001_001E, B17001_002E, pctPoverty) -> moPoverty

### combine spatial and geometric data
povertyMap <- left_join(moCounties, moPoverty, by = "GEOID")


jenks <- classIntervals(povertyMap$pctPoverty, n=5, style="jenks")
poverty <- cut(povertyMap$pctPoverty, breaks = c(jenks$brks))


## base map
base <- ggplot() + 
  geom_sf(data = mo, fill = "#ffffff", color = NA) + 
  geom_sf(data = povertyMap, mapping = aes(fill = poverty), color = NA) +
  geom_sf(data = mo, fill = NA, color = "#000000", size = .25) +
  scale_fill_brewer(palette = "BuGn", name = "Percent",
    labels = c("6.12 - 11.00", "11.01 - 15.70", "15.71 - 19.90", "19.91 - 24.20", "24.21 - 30.50")) +
  labs(
    title = "Poverty Rates by County, 2016",
    subtitle = "Population at or Federal Below Poverty Line in Missouri",
    caption = "Data via U.S. Census Bureau \nMap by Christopher Prener, Ph.D."
  ) 

## map with white background
map01 <- base +
  cp_sequoiaTheme(background = "white")

cp_plotSave(filename = "2016/results/map01-poverty.png", plot = map01, preset = "lg", dpi = 500)

## map with transparent background
map02 <- base +
  cp_sequoiaTheme(background = "transparent")

cp_plotSave(filename = "2016/results/map02-poverty.png", plot = map02, preset = "lg", dpi = 500)
