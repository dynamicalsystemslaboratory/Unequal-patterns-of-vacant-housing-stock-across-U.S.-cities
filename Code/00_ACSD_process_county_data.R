library(readxl)
library(dplyr)


setwd() #Path for working folder


#------------------------------------------------------------ DATA ---------------------------------------

# ACSD Total, Occupied and Vacant
file_paths_general <- c(
  "Data/ACSD house/county_general/ACSDT5Y2010.B25002-2025-03-29T192400.xlsx", 
  "Data/ACSD house/county_general/ACSDT5Y2011.B25002-2025-03-29T192340.xlsx", 
  "Data/ACSD house/county_general/ACSDT5Y2012.B25002-2025-03-29T192313.xlsx", 
  "Data/ACSD house/county_general/ACSDT5Y2013.B25002-2025-03-29T192228.xlsx",
  "Data/ACSD house/county_general/ACSDT5Y2014.B25002-2025-03-29T192159.xlsx", 
  "Data/ACSD house/county_general/ACSDT5Y2015.B25002-2025-03-29T192121.xlsx", 
  "Data/ACSD house/county_general/ACSDT5Y2016.B25002-2025-03-29T192045.xlsx", 
  "Data/ACSD house/county_general/ACSDT5Y2017.B25002-2025-03-29T192014.xlsx", 
  "Data/ACSD house/county_general/ACSDT5Y2018.B25002-2025-03-29T191945.xlsx", 
  "Data/ACSD house/county_general/ACSDT5Y2019.B25002-2025-03-29T191918.xlsx", 
  "Data/ACSD house/county_general/ACSDT5Y2020.B25002-2025-03-29T191848.xlsx", 
  "Data/ACSD house/county_general/ACSDT5Y2021.B25002-2025-03-29T191827.xlsx", 
  "Data/ACSD house/county_general/ACSDT5Y2022.B25002-2025-03-29T191811.xlsx", 
  "Data/ACSD house/county_general/ACSDT5Y2023.B25002-2025-03-29T191743.xlsx"
)

# ACSD Vacancy status
file_paths_vac_stat <- c(
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2010.B25004-2025-03-30T205000.xlsx", 
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2011.B25004-2025-03-30T205020.xlsx",
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2012.B25004-2025-03-30T205042.xlsx",
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2013.B25004-2025-03-30T205117.xlsx",
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2014.B25004-2025-03-30T205148.xlsx",
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2015.B25004-2025-03-30T205207.xlsx",
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2016.B25004-2025-03-30T205230.xlsx",
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2017.B25004-2025-03-30T205257.xlsx",
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2018.B25004-2025-03-30T212842.xlsx",
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2019.B25004-2025-03-30T205345.xlsx",
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2020.B25004-2025-03-30T205400.xlsx",
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2021.B25004-2025-03-30T205419.xlsx",
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2022.B25004-2025-03-30T203904.xlsx",
  "Data/ACSD house/county_vacancy_status/ACSDT5Y2023.B25004-2025-03-30T205435.xlsx"
)

#-------------------------------------------------------------------------------

ACSD_df <- data.frame()

# Loop
for (file in file_paths_general) {
  ACSD_temp <- read_excel(file, sheet = 2)

  ACSD_temp <- ACSD_temp %>%
    mutate(across(c(Total, Occupied, Vacant), ~ as.numeric(gsub(",", "", .))))
  
  estimate_rows <- which(ACSD_temp$Label == "Estimate")

  for (i in estimate_rows) {
    ACSD_temp[i-1, c("Total", "Occupied", "Vacant")] <- ACSD_temp[i, c("Total", "Occupied", "Vacant")]
  }

  ACSD_temp <- ACSD_temp %>%
    filter(Label != "Estimate")
  
  year <- substr(basename(file), 8, 11)
  ACSD_temp$year <- year

  ACSD_df <- bind_rows(ACSD_df, ACSD_temp)
}

dt <- read_excel("Data/ACSD house/county_vacancy_status/ACSDT5Y2010.B25004-2025-03-30T205000.xlsx", sheet = 2)
colnames()

ACSD_df_vac_stat <- data.frame()

# Loop
for (file in file_paths_vac_stat) {
  ACSD_temp <- read_excel(file, sheet = 2)
  
  ACSD_temp <- ACSD_temp %>%
    mutate(across(c(Total, For_rent, Rented_not_occupied, For_sale_only, Sold_not_occupied, For_seasonal_recreational_or_occasional_use, For_migrant_workers, Other_vacant), ~ as.numeric(gsub(",", "", .))))
      
  estimate_rows <- which(ACSD_temp$Label == "Estimate")
  
  for (i in estimate_rows) {
    ACSD_temp[i-1, c("Total", "For_rent", "Rented_not_occupied", "For_sale_only", "Sold_not_occupied",
                     "For_seasonal_recreational_or_occasional_use", "For_migrant_workers","Other_vacant")] <- ACSD_temp[i, c("Total", "For_rent", "Rented_not_occupied", "For_sale_only", "Sold_not_occupied",
                                                                                                                             "For_seasonal_recreational_or_occasional_use", "For_migrant_workers","Other_vacant")]
  }
  
  ACSD_temp <- ACSD_temp %>%
    filter(Label != "Estimate")
  
  year <- substr(basename(file), 8, 11)
  ACSD_temp$year <- year
  
  ACSD_df_vac_stat <- bind_rows(ACSD_df_vac_stat, ACSD_temp)
}

# Rename the Total column
ACSD_df_vac_stat <- ACSD_df_vac_stat %>% rename(Vacant = Total)


# Merge by Label and year, renaming "Total" to match "Vacant"
merged_df <- ACSD_df %>%
  left_join(ACSD_df_vac_stat, by = c("Label", "year", "Vacant"))



#saveRDS(merged_df, "ACSD_data_03302025.rds")







