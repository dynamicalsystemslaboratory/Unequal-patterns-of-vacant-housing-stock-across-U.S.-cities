library(sf)
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)


setwd("") # Path for working folder

# USPS info: https://www.huduser.gov/portal/datasets/usps.html

#---------------------------------------------------------------------------------------------------------
#------------------------------- MISC --------------------------------------------------------------------

process_usps <- function(file_path, year) {
  data_aux <- st_read(file_path) %>%
    rename_with(tolower) %>%  # Convert column names to lowercase
    mutate(County_code = substr(geoid, 1, 5)) %>%  # Extract county code
    select(County_code, geoid, ams_res, res_vac, nostat_res, vac_6_12r, vac_12_24r, vac_24_36r, vac_36_res, 
           ns_3_res, ns_3_6_res, ns_6_12_r, ns_12_24_r, ns_24_36_r, ns_36_res)  # Keep only relevant columns
  
  # Rename last two columns dynamically
  colnames(data_aux)[3:15] <- c(paste0("ams_res_", year), paste0("res_vac_", year), paste0("nostat_res", year), 
                                paste0("vac_6_12r", year), paste0("vac_12_24r", year), paste0("vac_24_36r", year), paste0("vac_36_res", year), 
                                paste0("ns_3_res", year),paste0("ns_3_6_res", year),paste0("ns_6_12_r", year),paste0("ns_12_24_r", year),paste0("ns_24_36_r", year),paste0("ns_36_res", year))
  
  # Summarize by County_code
  data <- data_aux %>%
    group_by(County_code) %>%
    summarize(
      !!paste0("ams_res_", year) := sum(!!sym(paste0("ams_res_", year)), na.rm = TRUE),
      !!paste0("res_vac_", year) := sum(!!sym(paste0("res_vac_", year)), na.rm = TRUE),
      !!paste0("nostat_res_", year) := sum(!!sym(paste0("nostat_res", year)), na.rm = TRUE),
      !!paste0("vac_6_12r_", year) := sum(!!sym(paste0("vac_6_12r", year)), na.rm = TRUE),
      !!paste0("vac_12_24r_", year) := sum(!!sym(paste0("vac_12_24r", year)), na.rm = TRUE),
      !!paste0("vac_24_36r_", year) := sum(!!sym(paste0("vac_24_36r", year)), na.rm = TRUE),
      !!paste0("vac_36_res_", year) := sum(!!sym(paste0("vac_36_res", year)), na.rm = TRUE),
      !!paste0("ns_3_res_", year) := sum(!!sym(paste0("ns_3_res", year)), na.rm = TRUE),
      !!paste0("ns_3_6_res_", year) := sum(!!sym(paste0("ns_3_6_res", year)), na.rm = TRUE),
      !!paste0("ns_6_12_r_", year) := sum(!!sym(paste0("ns_6_12_r", year)), na.rm = TRUE),
      !!paste0("ns_12_24_r_", year) := sum(!!sym(paste0("ns_12_24_r", year)), na.rm = TRUE),
      !!paste0("ns_24_36_r_", year) := sum(!!sym(paste0("ns_24_36_r", year)), na.rm = TRUE),
      !!paste0("ns_36_res_", year) := sum(!!sym(paste0("ns_36_res", year)), na.rm = TRUE),
      .groups = "drop"
    )
  
  return(data)
}



#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------------------
#------------------------------------------------------------ DATA ---------------------------------------

# USPS VACANCIES BY YEAR -------------------------------------------------------
# USPS data files for each year
USPS_2008 <- process_usps("Data/USPS/USPS_VAC_122008_TRACTSUM.dbf", "08")
USPS_2009 <- process_usps("Data/USPS/USPS_VAC_122009_TRACTSUM.dbf", "09")
USPS_2010 <- process_usps("Data/USPS/122010.dbf", "10")
USPS_2011 <- process_usps("Data/USPS/USPS_VAC_122011_TRACTSUM.dbf", "11")
USPS_2012 <- process_usps("Data/USPS/USPS_VAC_122012_TRACTSUM.dbf", "12")
USPS_2013 <- process_usps("Data/USPS/USPS_VAC_122013_TRACTSUM_2KX.dbf", "13")
USPS_2014 <- process_usps("Data/USPS/USPS_VAC_122014_TRACTSUM_2KX.dbf", "14")
USPS_2015 <- process_usps("Data/USPS/usps_vac_122015_tractsum_2kx.dbf", "15")
USPS_2016 <- process_usps("Data/USPS/usps_vac_122016_tractsum_2kx.dbf", "16")
USPS_2017 <- process_usps("Data/USPS/usps_vac_122017_tractsum_2kx.dbf", "17")
USPS_2018 <- process_usps("Data/USPS/usps_vac_122018_tractsum_2kx.dbf", "18")
USPS_2019 <- process_usps("Data/USPS/usps_vac_122019_tractsum_2kx.dbf", "19")
USPS_2020 <- process_usps("Data/USPS/usps_vac_122020_tractsum_2kx.dbf", "20")
USPS_2021 <- process_usps("Data/USPS/usps_vac_122021_tractsum_2kx.dbf", "21")
USPS_2022 <- process_usps("Data/USPS/usps_vac_122022_tractsum_2kx.dbf", "22")


# Population data reference
# https://repository.duke.edu/catalog/f49b199b-1496-4636-91f3-36226c8e7f80 (for POP2000)
county_pop00 <- read_excel("Data/POP2000.xlsx", col_names = TRUE)[, 1:7]
county_pop10 <- read_excel("Data/co-est2019-annres.xlsx", sheet = 2)
county_pop20 <- read.csv("Data/co-est2023-alldata.csv")





# ------------------------------------------------------------------------------
# County population 2000-2020 --------------------------------------------------
county_pop00 <- county_pop00[county_pop00$year != 2010, ]

county_pop10 <- county_pop10 %>% mutate(GeographicArea = gsub("^\\.", "", GeographicArea))

county_pop00 <- county_pop00 %>%
  mutate(GeographicArea = paste(ctyname, stname, sep = ", ")) %>%
  select(fips, GeographicArea, year, tot_pop) %>%
  pivot_wider(names_from = year, values_from = tot_pop, names_prefix = "POPESTIMATE") %>%
  select(fips, GeographicArea, starts_with("POPESTIMATE"))

merged_data <- full_join(county_pop00, county_pop10, by = "GeographicArea")


county_pop20 <- county_pop20 %>%
  filter(COUNTY != 0) %>%
  mutate(
    STATE = str_pad(STATE, width = 2, pad = "0"),
    COUNTY = str_pad(COUNTY, width = 3, pad = "0", side = "left"),
    County_code = paste(STATE, COUNTY, sep = "")
  )
county_pop20 <- county_pop20 %>%
  mutate(GeographicArea = paste(CTYNAME,", ",STNAME)) %>%
  select(-CTYNAME, -STNAME)

county_pop20 <- county_pop20 %>%
  mutate(GeographicArea = gsub("\\s*,\\s*", ", ", GeographicArea))

merged_data2 <- full_join(merged_data, county_pop20, by = "GeographicArea")
merged_data2 <- merged_data2 %>% mutate(County_code = ifelse(is.na(County_code), fips, County_code)) #County_code NA == fips

na_rows <- merged_data2 %>% filter_all(any_vars(is.na(.))) ## VIZ


#keep consistency with 2010 county areas
merged_data <- merged_data2[!is.na(merged_data2$POPESTIMATE2013), ]             
merged_data <- merged_data %>%
  select(-SUMLEV, -REGION, -DIVISION, -STATE, -COUNTY)  # columns to remove

## DEBUG------------------
na_rows <- merged_data %>% filter_all(any_vars(is.na(.))) ## VIZ

merged_data <- merged_data[!is.na(merged_data$fips), ]





# Reshape population data
merged_data_long <- merged_data %>%
  pivot_longer(
    cols = starts_with("POPESTIMATE"), # Select columns that start with "POPESTIMATE"
    names_to = "year",  # New column to store the year
    values_to = "POPESTIMATE"  # New column for the population estimates
  ) %>%
  mutate(year = as.integer(gsub("POPESTIMATE", "", year))) %>%
  arrange(year, County_code) %>%  # Ensure the data is ordered by County_code and year
  select(County_code, GeographicArea, year, POPESTIMATE)  # Select only the desired columns



# List all processed datasets
usps_list <- list(USPS_2008, USPS_2009, USPS_2010, USPS_2011, USPS_2012, USPS_2013, USPS_2014, USPS_2015, 
                  USPS_2016, USPS_2017, USPS_2018, USPS_2019, USPS_2020, USPS_2021, USPS_2022)

# Merge all datasets by County_code
USPS_df <- reduce(usps_list, full_join, by = "County_code")  

USPS_df <- USPS_df %>% 
  filter(!grepl("^(72|15)", County_code))


# Remove rows with NA or zero values in any column
USPS_df <- USPS_df %>%
  filter(across(everything(), ~ !is.na(.))) #REMOVE THE NA'S
# filter(across(everything(), ~ !is.na(.) & . != 0))

# Reshape USPS_df (THIS SHOULD BE THE LAST STEP)
USPS_long <- USPS_df %>%
  pivot_longer(
    cols = -County_code, 
    names_to = c("variable", "year"), 
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  mutate(year = as.integer(year) + 2000) %>%  # Convert year to 2008, 2009, etc.
  pivot_wider(
    names_from = "variable",
    values_from = "value",
    values_fn = list(value = first)  # Ensures no list-cols
  ) %>%
  select(County_code, year, ams_res, res_vac, nostat_res, vac_6_12r, vac_12_24r, vac_24_36r, vac_36_res, 
         ns_3_res, ns_3_6_res, ns_6_12_r, ns_12_24_r, ns_24_36_r, ns_36_res) %>%
  arrange(year, County_code)  # Ensures chronological order

USPS_long$res_occup <- USPS_long$ams_res - USPS_long$res_vac - USPS_long$nostat_res 


USPS_long_zero <- USPS_long %>% filter(ams_res == 0 | res_vac == 0 | nostat_res == 0)


# merge USPS with population, keeping all rows from USPS_long
USPS_pop <- USPS_long %>% 
  left_join(merged_data_long, by = c("County_code", "year"))

data <- USPS_pop 


# Save and load clean_data
saveRDS(data, file = "Data/clean_data.rds")
data <- readRDS("Data/clean_data.rds")






