library(sf)
library(dplyr)

### Download all Census tracts 2020 shapefiles from https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
# Accessed April 28th, 2026


#Does not include Alaska, Puerto Rico and Hawaii
shp_files_2020 <- list.files("Data\\tl_2020_states_tract", pattern = "\\.shp$", full.names = TRUE)

all_tracts_2020 <- lapply(shp_files_2020, st_read) %>%
  bind_rows()


#st_write(all_tracts_2020, "all_tracts_2020.shp") 







#Check
Tracts_2020_shp <- st_read("Data/tl_2020_states_tract/all_tracts_2020.shp")












