# before running this script for the first time, you may need to install the
# following packages. remove # in front of install.packages() and run. after 
# installation, you can comment out (#) or delete the install.packages() lines

# install.packages("dplyr")
# install.packages("RColorBrewer")
# install.packages("sf")
# install.packages("tmap")

# load packages

library(dplyr)
library(RColorBrewer)
library(sf)
library(tmap)

# set working directory

setwd("C:/Users/caraw/Documents/SSPHC/TAing/2023-2024/Term2/Lab2/lab2_asthma")

# read in California counties downloaded from: 
# https://www.arcgis.com/home/item.html?id=2f227372477d4cddadc0cd0b002ec657

cty <- st_read("CA_counties.shp")

# check that shapefile has the fields ‘NAME’ and ‘GEOID’ for merging

names(cty) # column names
head(cty) # preview data

# read in asthma ED visit rates downloaded from:
# https://data.chhs.ca.gov/dataset/asthma-ed-visit-rates-lghc-indicator-07

asthma <- read.csv("asthma-ed-visit-rates_2019.csv")

# filter the asthma data so that Year = 2019, Age Group = "All Ages", and 
# Strata = "Total Population"

asthma <- asthma %>%
  filter(Year == 2019 & Age.Group == "All Ages" & Strata == "Total Population")

nrow(asthma) # checks that there are 58 rows, 1 for each county in California

# merge county boundaries and asthma data using the county name variable

asthma_cty <- cty %>%
  left_join(asthma, by = c("NAME" = "Geography"))

# map asthma rates

map1 <- tm_shape(asthma_cty) +
  tm_fill("Rate", palette = "YlOrBr", style = "jenks", 
          title = "Asthma ED Visit Rate per 10,000 people") +
  tm_layout(legend.stack = "vertical",
            legend.outside = TRUE,
            legend.outside.position = "right") +
  tm_layout(main.title = "Asthma ED Visit Rates", 
            main.title.position = "center") +
  tm_borders("#4E4E4E", lwd = 1) +
  tm_layout(frame = FALSE) +
  tm_compass(text.size = 0.75, position = c("center", "top")) +
  tm_scale_bar(text.size = 0.75, position = c("right", "top"))

map1

# read in power plants downloaded from:
# https://atlas.eia.gov/datasets/power-plants/

plants <- st_read("Power_Plants.shp")

# select only power plants in California with PrimSource coal or petroleum

plants <- plants %>%
  filter(State == "California" & PrimSource %in% c("coal", "petroleum"))

# create new object: select only power plants with PrimSource = coal

coal <- plants %>%
  filter(PrimSource == "coal")

# create new object: select only power plants with PrimSource = petroleum

petroleum <- plants %>%
  filter(PrimSource == "petroleum")

# overlay coal and petroleum plant locations on map of asthma rates

map2 <- map1 +
  tm_shape(coal) +
  tm_dots(size = 0.25, shape = 24) +
  tm_shape(petroleum) +
  tm_dots(size = 0.25, shape = 22) +
  tm_add_legend(type = "symbol",
                labels = c("Coal", "Petroleum"),
                shape = c(24, 22),
                col = "black",
                size = c(0.25, 0.25),
                title = "Power Plant Type",
                z = 0)

map2

# read in air pollution data from:
# https://aqs.epa.gov/aqsweb/airdata/download_files.html

nox <- read.csv("annual_conc_by_monitor_2019.csv")

# filter to California and NOx

nox <- nox %>%
  filter(State.Name == "California" & 
           Parameter.Name == "Oxides of nitrogen (NOx)") %>%
  select(Longitude, Latitude, Arithmetic.Mean)
  
# covert to spatial dataframe using ‘Longitude,’ ‘Latitude,’ and 
# ‘Arithmetic.Mean’ as the parts per billion annual concentration of NOx

nox <- nox %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = "EPSG:4326")

# finally, add NOx to map and update map title

map3 <- map2 +
  tm_shape(nox) +
  tm_dots("Arithmetic.Mean", 
          style = "jenks",
          size = 0.25,
          pal = "Blues",
          title = "NOx Mean Conc. (ppb)",
          legend.z = 1) +
  tm_layout(main.title = "Location of Power Plants and NOx Monitors\n Overlaid with Astha ED Visit Rates", 
            main.title.position = "center") 

map3

# count the number of coal or petroleum plants within each county AND calculate
# the average NOx concentration in each county
# note that we're performing two spatial joins here. First, st_intersects() along 
# with lengths() calculates the number of plants in each county. Second, st_join() 
# spatially joins counties and NOx points. Some counties have multiple NOx points,
# and st_join() creates extra rows for each unique county-NOx join. That is why we 
# then use group_by() to group by county and calculate the average NOx. Finally, 
# distinct() removes those duplicates rows so that we're again left with 58 
# counties. There are other ways to do a points-in-polygon spatial join along 
# with statistical calculations like mean in R. The aggregate() function can also
# accomplish this. 

asthma_cty <- asthma_cty %>%
  mutate(N_Plants = lengths(st_intersects(., plants))) %>%
  st_join(nox) %>%
  group_by(NAME) %>%
  mutate(Avg_NOx = mean(Arithmetic.Mean, na.rm = TRUE)) %>%
  ungroup() %>%
  as.data.frame() %>%
  select(-c(Arithmetic.Mean)) %>%
  distinct(NAME, .keep_all = TRUE) %>%
  st_as_sf() 

# create a binary field indicating whether a county has or doesn't have a power plant

asthma_cty$Has_Plant <- ifelse(asthma_cty$N_Plants > 0, 1, 0)

sum(asthma_cty$Has_Plant) # 8 counties have plants

# create an object containing only counties that have average NOx data available

nox_cty <- asthma_cty %>%
  filter(!is.na(Avg_NOx))

nrow(nox_cty) # 32 counties have NOx data

# create Table 1

nox_cty %>%
  as.data.frame() %>%
  group_by(Has_Plant) %>%
  summarise(Number_of_Counties = n(),
            ER_Rate_Mean = mean(Rate),
            ER_Rate_Median = median(Rate),
            ER_Rate_Min = min(Rate),
            ER_Rate_Max = max(Rate),
            NOx_Mean = mean(Avg_NOx),
            NOx_Median = median(Avg_NOx),
            NOx_Min = min(Avg_NOx),
            NOx_Max = max(Avg_NOx)) %>%
  as.data.frame()
