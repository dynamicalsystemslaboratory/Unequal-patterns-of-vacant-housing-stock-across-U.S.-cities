library(stringr)
library(dplyr)
library(sf)
library(patchwork)
library(extrafont)
library(ggplot2)
library(sfdep) # https://rdrr.io/cran/sfdep/man/spatial_gini.html (Accessed April 28th, 2026)
library(readxl)
library(maps)

loadfonts(device = "win") 

setwd("") # Path for working folder  


#---------------------------------------------------------------------------------------------------------
#------------------------------------------------------------ DATA ---------------------------------------
### Load Data
data <- readRDS("Data/clean_data.rds")
ACSD_data <- readRDS("Data/ACSD house/ACSD_data_clean.rds")
Census2020 <- read_excel("Data/DECENNIALPL2020.H1-2025-03-26T211423.xlsx", sheet = 2)

# Source: U.S. Census Bureau, 2020 Census Redistricting Data (Public Law 94-171) (https://data.census.gov/table/DECENNIALDHC2020.H5?q=Vacant+houses+H5&g=010XX00US$1400000)
Census20_tract <- readRDS("Data/Census20_tract_data_04022025.rds")

# Crosswalks 
MSA <- read_excel("Data/qcew-county-msa-csa-crosswalk.xlsx", sheet = 3)


cbsa_shp20 <- st_read("Data/tl_2020_us_cbsa/tl_2020_us_cbsa.shp")

# Dataset obtained using "Process tracts shp.R" code
tracts_shp20 <- st_read("Data/tl_2020_states_tract/all_tracts_2020.shp")




# Data processing ----------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------


# Select only the tracts that are part of MSAs
Census20_tract <- Census20_tract[!is.na(Census20_tract$MAS_Code), ]

# Remove Alaska, Puerto Rico and Hawaii
Census20_tract <- Census20_tract %>% filter(!grepl("Alaska|Puerto Rico|Hawaii", GeographicAreaName))

#Create a column with the tract
Census20_tract  <- Census20_tract  %>% mutate(Tract_code = str_extract(Geography, "(?<=US)\\d+$"))


# Column with MAS_Code
cbsa_shp20 <- cbsa_shp20 %>% 
  mutate(MAS_Code = paste0("C", substr(GEOID, 1, 4))) %>%
  rename(geometry_MSA = geometry) %>%
  select(MAS_Code, geometry_MSA)   # <-- keep only what you need


# Merge the datasets by 'MAS_Code', keeping all rows from Census20_tract
df <- Census20_tract %>% left_join(cbsa_shp20, by = "MAS_Code")


# Merge with the tracts shapefile keeping the rows from df
df <- df %>% 
  left_join(tracts_shp20, by = c("Tract_code" = "GEOID")) %>% 
  rename(geometry_tract = geometry)



# ------------------------------------------------------------------------------
## Spatial Gini ----------------------------------------------------------------

# Remove rows where 'Total' is 0
df <- subset(df, Total != 0)

# Do spatial Gini using vacancy rate to account for differences between tracts
df$Vacant_rate <- df$Total_Vacant / df$Total
df$Other_vacant_rate <- df$Other_vacant / df$Total


df <- st_as_sf(df)







### Compute Moran's I and Gini -------------------------------------------------
# ------------------------------------------------------------------------------

Gini_list <- list()
i <- 1

for (mas_code in unique(df$MAS_Code)) {

  mas_data <- df %>% filter(MAS_Code == mas_code)
  mas_data_sf <- st_as_sf(mas_data) %>%
  st_set_geometry("geometry_tract") %>%
  st_make_valid()

  nb <- spdep::poly2nb(mas_data_sf, queen = TRUE)
  lw <- spdep::nb2listw(nb, style = "B", zero.policy = TRUE)

  MoransI_vacant_rate <- spdep::moran.test(mas_data_sf$Vacant_rate, lw, zero.policy = TRUE)$estimate[1]
  MoransI_other_vacant_rate <- spdep::moran.test(mas_data_sf$Other_vacant_rate, lw, zero.policy = TRUE)$estimate[1]

  # remove islands
  has_neighbors <- spdep::card(nb) > 0
  nb_clean <- spdep::subset.nb(nb, has_neighbors)

  vacant_clean <- mas_data_sf$Vacant_rate[has_neighbors]
  other_vacant_clean <- mas_data_sf$Other_vacant_rate[has_neighbors]


  # Gini
  g_v  <- sfdep::spatial_gini(vacant_clean, nb_clean)
  g_ov <- sfdep::spatial_gini(other_vacant_clean, nb_clean)

  Gini_list[[i]] <- data.frame(
     MAS_Code = mas_code,
     MoransI_vacant_rate = MoransI_vacant_rate,
     MoransI_other_vacant_rate = MoransI_other_vacant_rate,
     G_vacant_rate  = g_v[1],
     NG_vacant_rate = g_v[2],
     NBG_vacant_rate= g_v[3],
     SG_vacant_rate = g_v[4],
     G_other_vacant_rate  = g_ov[1],
     NG_other_vacant_rate = g_ov[2],
     NBG_other_vacant_rate= g_ov[3],
     SG_other_vacant_rate = g_ov[4]
  )
   i <- i + 1
 }

 SG_results <- dplyr::bind_rows(Gini_list)
 colnames(SG_results) <- c(
   "MAS_Code",
   "MoransI_vacant_rate",
   "MoransI_other_vacant_rate",
   "G_vacant_rate",
   "NG_vacant_rate",
   "NBG_vacant_rate",
   "SG_vacant_rate",
   "G_other_vacant_rate",
   "NG_other_vacant_rate",
   "NBG_other_vacant_rate",
   "SG_other_vacant_rate")

#saveRDS(SG_results, "Data/Binary_SG_results_04082026") #Save
#SG_results <- readRDS("Data/Binary_SG_results_04082026") # Load 



###-------------------------------------------------------------
# G: the Gini index
# NBG: the neighbor composition of the Gini coefficient
# NG: the non-neighbor composition of the Gini coefficient
# SG: the Spatial Gini which is equal to NG * \frac{1}{G}
###-------------------------------------------------------------


# Aggegated results by MSA 
MSA20 <- df %>%
  group_by(MAS_Code) %>%
  summarise(
    MSA_title = first(MAS_Title),
    Total = sum(Total, na.rm = TRUE),
    Other_vacant = sum(Other_vacant, na.rm = TRUE),
    vacant_rate = sum(Total_Vacant, na.rm = TRUE) / sum(Total, na.rm = TRUE),
    other_vacant_rate = sum(Other_vacant, na.rm = TRUE) / sum(Total, na.rm = TRUE),
    population = sum(Population, na.rm = TRUE),
    geometry_MSA = first(geometry_MSA)
  ) %>%
  st_as_sf()


#Merge MSA20 with SG_results
MSA20 <- MSA20 %>% left_join(SG_results, by = c("MAS_Code" = "MAS_Code"))


# Sort SG_vacant_rate in descending order
MSA20 <- MSA20[order(MSA20$SG_vacant_rate, decreasing = TRUE),]
# Compute the cumulative distribution
MSA20$CDF_SG_vacant_rate <- 1 - (seq_along(MSA20$SG_vacant_rate) / length(MSA20$SG_vacant_rate))






### Plots Spatial Gini (SG) vacancy rate ---------------------------------------

# Plot Spatial Gini CDF
p_CDF <- ggplot(MSA20, aes(x = SG_vacant_rate, y = CDF_SG_vacant_rate)) +
  geom_line(color = "red", size = 0.7) +
  labs(x = "Spatial Gini Index", y = "CDF") +
  scale_x_continuous(limits = c(0.5, 1), expand = c(0, 0)) + ### Adjust as needed
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_bw(7) +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    text = element_text(family = "Arial")
  )
p_CDF 
#ggsave(filename = "SG_MSA_Gini_CDF.pdf", plot = p_CDF , width = 45, height = 45, units = "mm", dpi = 900, device = cairo_pdf)


# Plot Spatial Gini PDF
p_PDF <- ggplot(MSA20 , aes(x = SG_vacant_rate)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(x = "Spatial Gini Index", y = "Probability density") +
  scale_x_continuous(limits = c(0.5, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0)) +
  theme_bw(7)+
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"), #t,r,b,l
    text = element_text(family = "Arial")
  )
p_PDF
#ggsave(filename = "SG_MSA_Gini_PDF.pdf", plot = p_PDF , width = 45, height = 45, units = "mm", dpi = 900, device = cairo_pdf)


# Plot Gini PDF 
p_PDF_Gini <- ggplot(MSA20 , aes(x = G_vacant_rate)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(x = "Gini Index", y = "Probability density") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +
  theme_bw(7)+
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"), #t,r,b,l
    text = element_text(family = "Arial")
  )
p_PDF_Gini
#ggsave(filename = "Gini_MSA_Gini_PDF.pdf", plot = p_PDF_Gini, width = 45, height = 45, units = "mm", dpi = 900, device = cairo_pdf)




### Maps -----------------------------------------------------------------------
us_map <- map_data("state")


# Plot vacancy rate
p_rate <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = NA, color = "darkgrey") +
  geom_sf(data = MSA20, aes(fill = vacant_rate)) + 
  scale_fill_viridis_c(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), oob = scales::squish) +
  labs(x = "Longitude", y = "Latitude", fill = "Vacancy") +
  theme_minimal(7) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    legend.position = c(0.9, 0.2),
    legend.title = element_text(size = 7),
    legend.text  = element_text(size = 6),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing = unit(0.1, "cm"),
    legend.background = element_rect(fill = "white", color = "white", text = element_text(family = "Arial"))  
  )
#p_rate 


p_Gini <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = NA, color = "darkgrey") + # US border
  geom_sf(data = MSA20, aes(fill = G_vacant_rate)) + 
  scale_fill_viridis_c(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), oob = scales::squish) +
  labs(x = "Longitude", y = "Latitude", fill = "Gini") +
  theme_minimal(7) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    legend.position = c(0.9, 0.2),
    legend.title = element_text(size = 7),
    legend.text  = element_text(size = 6),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing = unit(0.1, "cm"),
    legend.background = element_rect(fill = "white", color = "white",
                                     text = element_text(family = "Arial"))  
  )
#p_Gini


# Plot SGI
p_NG <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = NA, color = "darkgrey") + # US border
  geom_sf(data = MSA20, aes(fill = NG_vacant_rate)) +  
  scale_fill_viridis_c(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), oob = scales::squish) +
  labs(fill = "NG") +
  theme_minimal(7) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    legend.position = c(0.9, 0.2),
    legend.title = element_text(size = 7),
    legend.text  = element_text(size = 6),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing = unit(0.1, "cm"),
    legend.background = element_rect(fill = "white", color = "white",
                                     text = element_text(family = "Arial"))  
  )
#p_NG 


# Plot SGI
p_SGI <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = NA, color = "darkgrey") + # US border
  geom_sf(data = MSA20, aes(fill = SG_vacant_rate)) +  
  scale_fill_viridis_c(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), oob = scales::squish) +
  labs(fill = "SGI") +
  theme_minimal(7) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    legend.position = c(0.9, 0.2),
    legend.title = element_text(size = 7),
    legend.text  = element_text(size = 6),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing = unit(0.1, "cm"),
    legend.background = element_rect(fill = "white", color = "white",
                                     text = element_text(family = "Arial"))  
  )
#p_SGI 


p <- (p_Gini / p_SGI ) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 12, face = "bold"),
    theme(text = element_text(family = "Arial"))
  )
p
#ggsave(filename = "SI_Fig23_maps.pdf", plot = p, width = 110, height = 110, units = "mm", dpi = 900, device = cairo_pdf)









































































