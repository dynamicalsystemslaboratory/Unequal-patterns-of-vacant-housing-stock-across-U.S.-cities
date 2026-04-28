library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(readxl)
library(extrafont)
library(patchwork)
library(sandwich)

loadfonts(device = "win") 

setwd("")  # Path for working folder


#---------------------------------------------------------------------------------------------------------
#------------------------------------------------------------ DATA ---------------------------------------
MSA <- read_excel("Data/qcew-county-msa-csa-crosswalk.xlsx", sheet = 3)

### Urban Area
# Criteria: For the 2020 Census, an urban area must have a minimum of 5,000 people or 2,000 housing units.
ua20_shp <- st_read("Data/tl_2020_us_uac20/tl_2020_us_uac20.shp") 

### Urban Areas to county from Census
# https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural.html

UA_COUNTY <- read_excel("Data/2020_UA_COUNTY.xlsx", sheet = 2)

cbsa_shp20 <- st_read("Data_/tl_2020_us_cbsa")


### Housing from Decennial Census 2020 - Urban Areas
# https://data.census.gov/table/DECENNIALDHC2020.H3?t=Housing+Units:Vacancy&g=010XX00US$4000000&y=2020&tp=false
urbanAreas_df <- read.csv("Data/DECENNIALDHC2020.H3-2026-02-25T015753.csv")


### Population from Decennial Census 2020 - Urban Areas
# https://data.census.gov/table/DECENNIALDHC2020.P1?t=Population+Total&g=010XX00US$4000000&y=2020&tp=true
pop_urbanAreas_df <- read.csv("Data/DECENNIALDHC2020.P1-Data.csv")



# Data processing ----------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------


# Clean Population
pop_clean <- pop_urbanAreas_df %>%
  filter(!is.na(P1_001N)) %>%
  filter(str_detect(P1_001N, "^[0-9,]+$")) %>%
  mutate(
    GEOID = GEO_ID,
    Label = NAME,
    Population = as.numeric(gsub(",", "", P1_001N))
  ) %>%
  select(GEOID, Label, Population)


# Clean housing
housing_clean <- urbanAreas_df %>%
  mutate(
    Total. = na_if(Total., ""),
    Total...Occupied = na_if(Total...Occupied, ""),
    Total...Vacant = na_if(Total...Vacant, "")
  ) %>%
  filter(!is.na(Total...Occupied)) %>%
  mutate(
    Label = str_trim(Label..Grouping.),
    Total = as.numeric(gsub(",", "", Total.)),
    Occupied = as.numeric(gsub(",", "", Total...Occupied)),
    Vacant = as.numeric(gsub(",", "", Total...Vacant))
  ) %>%
  select(Label, Total, Occupied, Vacant)

# Merge population and housing
housing_urbanAreas_df <- pop_clean %>%
  inner_join(housing_clean, by = "Label") %>%
  mutate(
    state = str_extract(Label, "(?<=, )[A-Z]{2}(?= Urban Area)")
  ) %>%
  filter(!state %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS")) %>%
  select(-state) %>%
  mutate(
    GEOID = str_sub(GEOID, -5)
  ) %>%
  filter(
    !is.na(Population),
    !is.na(Vacant),
    !is.na(Total),
    !is.na(Occupied),
    Population > 0,
    Vacant > 0
  )

# Merge housing data to urban area
housing_ua <- ua20_shp %>%
  left_join(housing_urbanAreas_df, by = c("GEOID20" = "GEOID")) %>%
  filter(
    !is.na(Population),
    !is.na(Vacant),
    Population > 0,
    Vacant > 0
  ) %>%
  mutate(ua_id = row_number())


# Match each urban area to primary MSA by largest overlap --------

# Keep only metropolitan CBSAs
cbsa_metro <- cbsa_shp20 %>%
  filter(grepl("Metro Area", NAMELSAD)) %>%
  select(primary_MSA = GEOID, geometry)

# Keep only columns needed for spatial operation
housing_ua_small <- housing_ua %>%
  select(ua_id, GEOID20, Population, Total, Occupied, Vacant, geometry)



# Project to equal-area CRS before intersection/area calculations
# EPSG:5070
housing_ua_small <- housing_ua_small %>%
  st_make_valid() %>%
  st_transform(5070)

cbsa_metro <- cbsa_metro %>%
  st_make_valid() %>%
  st_transform(5070)


# Identify which MSAs each urban area intersects
intersections <- st_intersects(housing_ua_small, cbsa_metro)

housing_ua_small$point_MSA <- lengths(intersections)



# Store all matched MSAs as comma-separated string
housing_ua_small$matched_MSA <- sapply(intersections, function(idx) {
  if (length(idx) == 0) return(NA_character_)
  paste(cbsa_metro$primary_MSA[idx], collapse = ",")
})

# Keep only urban areas that intersect at least one MSA
housing_ua_overlap <- housing_ua_small %>%
  filter(point_MSA > 0)



# Compute intersection polygons only for relevant urban areas
int <- st_intersection(
  housing_ua_overlap,
  cbsa_metro
) %>%
  mutate(
    overlap_area = as.numeric(st_area(geometry))
  )

# Keep the MSA with largest overlap for each urban area
primary_match <- int %>%
  st_drop_geometry() %>%
  group_by(ua_id) %>%
  slice_max(overlap_area, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ua_id, primary_MSA, overlap_area)



housing_ua <- housing_ua %>%
  left_join(
    housing_ua_small %>%
      st_drop_geometry() %>%
      select(ua_id, point_MSA, matched_MSA),
    by = "ua_id"
  ) %>%
  left_join(primary_match, by = "ua_id")



# Save
#saveRDS(housing_ua, "Data/housing_ua_matched_to_MSA.rds")



### ----------------------------------------------------------------------------
### Scaling analysis -----------------------------------------------------------


# Group UA by MSA
housing_ua_MSA <- housing_ua %>%
  filter(point_MSA > 0) %>%
  group_by(primary_MSA) %>%
  summarise(
    Population = sum(Population, na.rm = TRUE),
    Total = sum(Total, na.rm = TRUE),
    Occupied = sum(Occupied, na.rm = TRUE),
    Vacant = sum(Vacant, na.rm = TRUE),
    # ALAND = sum(ALAND, na.rm = TRUE),
    # AWATER = sum(AWATER, na.rm = TRUE),
    n_ua = n()  # number of urban areas per MSA
    #geometry   = st_union(geometry)
  ) %>%
  ungroup()

# # Check all entries that have 0 population

housing_ua_MSA <- housing_ua_MSA[housing_ua_MSA$Population > 0, ]



### Plot with Urban Areas scaling ----------------------------------------------
model.vacant <- summary(lm(log10(Vacant) ~ log10(Population), data = housing_urbanAreas_df))
lmtest::bptest(lm(log10(Vacant) ~ log10(Population), data = housing_urbanAreas_df)) 

mod <- lm(log10(Vacant) ~ log10(Population), data = housing_urbanAreas_df)
ci <- confint(lmtest::coeftest(mod, vcov = vcovHC(mod, type="HC2")), level = 0.95)
ci

### Plot
ua_vacant_p <- ggplot(housing_urbanAreas_df, aes(x = Population, y = Vacant)) +
  geom_point(size = 0.5, alpha = 0.7, color = "#9b536d") +
  geom_abline(intercept = model.vacant$coefficients[1, 1], 
              slope = model.vacant$coefficients[2, 1], 
              linewidth = 0.5, alpha = 1) + 
  geom_abline(#intercept = model.vacant$coefficients[1, 1], 
    intercept = -1,
    slope = 1, linetype = "dashed", 
    linewidth = 0.5, alpha = 0.6) +
  xlab("Population") + 
  ylab("Vacant housing units") + 
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^2, 10^8), expand = c(0, 0)) + 
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^1, 10^7), expand = c(0, 0)) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"), #t,r,b,l
    legend.position = "none"
  )+
  theme(text = element_text(family = "Arial"))
ua_vacant_p




### ---------------------------------------------------------------------------- 
### Plot with Urban Areas scaling grouped by primary MSA -----------------------

model.vacant <- summary(lm(log10(Vacant) ~ log10(Population), data = housing_ua_MSA))
lmtest::bptest(lm(log10(Vacant) ~ log10(Population), data = housing_ua_MSA)) 

mod <- lm(log10(Vacant) ~ log10(Population), data = housing_ua_MSA)
ci <- confint(lmtest::coeftest(mod, vcov = vcovHC(mod, type = "HC2")), level = 0.95)
ci

# Plot
MSAua_vacant_p <- ggplot(housing_ua_MSA, aes(x = Population, y = Vacant)) +
  geom_point(size = 0.5, alpha = 0.7, color = "#9b536d") +
  geom_abline(intercept = model.vacant$coefficients[1, 1], 
              slope = model.vacant$coefficients[2, 1], 
              linewidth = 0.5, alpha = 1) + 
  geom_abline(#intercept = model.vacant$coefficients[1, 1], 
    intercept = -1,
    slope = 1, linetype = "dashed", 
    linewidth = 0.5, alpha = 0.6) +
  xlab("Population") + 
  ylab("Vacant housing units") + 
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^4, 10^8), expand = c(0, 0)) + 
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^1, 10^7), expand = c(0, 0)) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"), #t,r,b,l
    legend.position = "none"
  )+
  theme(text = element_text(family = "Arial"))
MSAua_vacant_p


p <- (ua_vacant_p | MSAua_vacant_p) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 11, face = "bold"),
    theme(text = element_text(family = "Arial"))
  )
p
#ggsave(filename = "SI_Fig_3.pdf", plot = p, width = 180, height = 75, units = "mm", dpi = 900, device = cairo_pdf)









