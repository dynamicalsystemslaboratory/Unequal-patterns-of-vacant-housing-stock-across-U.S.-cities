library(dplyr)
library(sf)
library(ggplot2)
library(readxl)
library(patchwork)
library(extrafont)
library(Cairo)
library(sandwich)
library(viridis)
library(scales)
library(RColorBrewer)
library(cowplot)
library(ggrepel)
library(rcartocolor)


loadfonts(device = "win", quiet = TRUE)
loadfonts(device = "win") 
fonts()  # List of available fonts


setwd("") # Path for working folder

#---------------------------------------------------------------------------------------------------------
#------------------------------------------------------------ DATA ---------------------------------------
data <- readRDS("Data/clean_data.rds")
ACSD_data <- readRDS("Data/ACSD house/ACSD_data_clean.rds")
Census2020 <- read_excel("Data/DECENNIALPL2020.H1-2025-03-26T211423.xlsx", sheet = 2)


# Crosswalks 
MSA <- read_excel("Data/qcew-county-msa-csa-crosswalk.xlsx", sheet = 3)


states_shp <- st_read("Data/tl_2022_us_state") %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS"))

# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2022.html#list-tab-790442341
county_shp22 <- st_read("Data/tl_2022_us_county")
cbsa_shp22 <- st_read("Data/tl_2023_us_cbsa/tl_2023_us_cbsa.shp")



# Data processing ----------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

# Remove Alaska, Puerto Rico and Hawaii
data <- subset(data, !grepl("Puerto Rico|Hawaii|Alaska", GeographicArea))

# Merge ACSD_df
data_aux <- data[data$year == 2022, ]

df <- merge(data_aux, ACSD_data, 
            by.x = c("GeographicArea", "year"), 
            by.y = c("Label", "year"), 
            all.x = TRUE)


# Filter rows where the last three characters of `MSA Title` are "MSA"
MSA <- MSA[grep("MSA$", MSA$MAS_Title), ]

#Select only counties that are part of MSAs
dfMSA_aux <- merge(df, MSA, by.x = "County_code", by.y = "County_Code", all.x = TRUE)
dfMSA_aux <- dfMSA_aux[!is.na(dfMSA_aux$MAS_Code), ]  #todos teem population dif 0


sum(is.na(dfMSA_aux$POPESTIMATE))
dfMSA_aux[is.na(dfMSA_aux$POPESTIMATE), ]


dfMSA_aux <- dfMSA_aux %>%
  group_by(MAS_Code) %>%
  summarize(
    MSA_tittle = first(MAS_Title),
    ams_res = sum(ams_res, na.rm = TRUE),
    res_vac = sum(res_vac, na.rm = TRUE),
    Vacant = sum(Vacant, na.rm = TRUE),
    For_seasonal_recreational_or_occasional_use = sum(For_seasonal_recreational_or_occasional_use, na.rm = TRUE),
    Other_vacant = sum(Other_vacant, na.rm = TRUE),
    Occupied = sum(Occupied, na.rm = TRUE),
    Total = sum(Total, na.rm = TRUE),
    POPESTIMATE = sum(POPESTIMATE, na.rm = TRUE),
    .groups = "drop"
  )


#Remove last zero from shapefile
cbsa_shp22 <- cbsa_shp22 %>%
  mutate(GEOID = paste0("C", substr(GEOID, 1, nchar(GEOID) - 1)))

# Merge with shapefile
df_MSA <- cbsa_shp22 %>%
  left_join(dfMSA_aux, by = c("GEOID" = "MAS_Code"))

df_MSA <- df_MSA %>% filter(!is.na(POPESTIMATE))


df_MSA$perCapita_vac <- df_MSA$Vacant / df_MSA$POPESTIMATE
df_MSA$perCapita_occup <- df_MSA$Occupied / df_MSA$POPESTIMATE

df_MSA$ratio_vac <- (df_MSA$Vacant / df_MSA$Total) * 100
df_MSA$ratio_vac_noSeasonal <-  ((df_MSA$Vacant - df_MSA$For_seasonal_recreational_or_occasional_use) / df_MSA$Total) * 100
df_MSA$ratio_vac_other <-  (df_MSA$Other_vacant / df_MSA$Total) * 100

df_MSA$ratio_vac_USPS <-  (df_MSA$res_vac / df_MSA$ams_res) * 100



#----
# Match CRS
states_shp <- st_transform(states_shp, st_crs(df_MSA))


# Ensure centroids are inside
df_MSA <- df_MSA %>%
  mutate(centroid = st_point_on_surface(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2])

# Color
reds <- carto_pal(50, "BrwnYl")

custom_breaks <- c(0.012, 0.036, 0.108, 0.28) 

# Plot
map_population <- ggplot() +
  geom_sf(data = states_shp, fill = "white", color = "black") +
  geom_point(
    data = df_MSA,
    aes(x = x, y = y, size = POPESTIMATE, fill = perCapita_vac),
    shape = 21, color = "lightgrey", alpha = 0.8
  ) +
  scale_fill_gradientn(
    colors = reds,
    name = "Vacant housing units per capita",
    trans = "log",
    na.value = "blue",
    breaks = custom_breaks,
    guide = "none"
  ) +
  scale_size(
    range = c(1, 11),
    labels = scales::comma,  
    guide = guide_legend(
      override.aes = list(fill = "#999090", color = "white", shape = 21, alpha = 1)
    )
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.8),
    text = element_text(family = "Arial")
  ) +
  labs(size = "Population")


# Dummy plot for legend
map_fill_legend <- ggplot(data = df_MSA, aes(x = x, y = y, fill = perCapita_vac)) +
  geom_point(shape = 21, size = 5) +
  scale_fill_gradientn(colors = reds, name = "Vacant housing units per capita", trans = "log", breaks = custom_breaks) +
  theme_void() +
  theme_classic(base_size = 8) +
  guides(
    fill = guide_colorbar(
      barwidth = 7, 
      barheight = 0.4,
      title.position = "top",
      direction = "horizontal")) +
  theme(text = element_text(family = "Arial"))

# Get legend
fill_legend <- get_legend(map_fill_legend)

# Combine map + bottom-left legend
final_p <- ggdraw() +
  draw_plot(map_population) +
  draw_plot(fill_legend, x = 0.01, y = 0.02, width = 0.4, height = 0.15, hjust = 0, vjust = 0)

final_p

##ggsave(filename = "Fig1_a.pdf", plot = final_p, width = 120, height = 80, units = "mm", dpi = 900, device = cairo_pdf)


### Plot with scaling ----------------------------------------------------------
model.vacant <- summary(lm(log10(Vacant) ~ log10(POPESTIMATE), data = df_MSA))
lmtest::bptest(lm(log10(Vacant) ~ log10(POPESTIMATE), data = df_MSA)) 

#mod <- lm(log10(Total) ~ log10(POPESTIMATE), data = df_MSA)
#ci <- confint(lmtest::coeftest(mod, vcov = vcovHC(mod, type="HC2")), level = 0.95)


### Plot
vacant_plot <- ggplot(df_MSA, aes(x = POPESTIMATE, y = Vacant)) +
  geom_point(size = 0.8, alpha = 0.7, color = "#9b536d") +
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
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^3, 10^7), expand = c(0, 0)) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.4, 0.4, 0.2, 0.2, "cm"), #t,r,b,l
    legend.position = "none"
  )+
  theme(text = element_text(family = "Arial"))
vacant_plot

#ggsave(filename = "Fig_b.pdf", plot = vacant_plot, width = 60, height = 60, units = "mm", dpi = 900, device = cairo_pdf)



#-------------------------------------------------------------------------------
### Occupied Plots -------------------------------------------------------------

# Color
greens <- colorRampPalette(c("#e5ff9a", "#013220"))(50)

custom_breaks <- c(0.263, 0.313, 0.372, 0.444) 
#range_vals <- c(0.263, 0.444)
#custom_breaksAUX <- exp(seq(log(range_vals[1]), log(range_vals[2]), length.out = 4))  # Get 4 evenly spaced breaks on the log scale
#custom_breaksAUX

# Plot
map_population <- ggplot() +
  geom_sf(data = states_shp, fill = "white", color = "black") +
  geom_point(data = df_MSA, aes(x = x, y = y, size = POPESTIMATE, fill = perCapita_occup), shape = 21, color = "lightgrey", alpha = 0.8) +
  scale_fill_gradientn(colors = greens, name = "Occupied housing units per capita", trans = "log",
    na.value = "blue", breaks = custom_breaks, guide = "none") +
  scale_size(range = c(1, 11), labels = scales::comma,  
    guide = guide_legend(
      override.aes = list(fill = "#999090", color = "white", shape = 21, alpha = 1)
    )
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.8),
    text = element_text(family = "Arial")
  ) +
  labs(size = "Population")


# Dummy plot for legend
map_fill_legend <- ggplot(data = df_MSA, aes(x = x, y = y, fill = perCapita_occup)) +
  geom_point(shape = 21, size = 5) +
  scale_fill_gradientn(colors = greens, name = "Occupied housing units per capita", trans = "log", breaks = custom_breaks) +
  theme_void() +
  theme_classic(base_size = 8) +
  guides(
    fill = guide_colorbar(
      barwidth = 7, 
      barheight = 0.4,
      title.position = "top",
      direction = "horizontal")) +
  theme(text = element_text(family = "Arial"))

# Get legend
fill_legend <- get_legend(map_fill_legend)

# Combine map + bottom-left legend
final_p <- ggdraw() +
  draw_plot(map_population) +
  draw_plot(fill_legend, x = 0.01, y = 0.02, width = 0.4, height = 0.15, hjust = 0, vjust = 0)

final_p

#ggsave(filename = "Fig1_c.pdf", plot = final_p, width = 120, height = 80, units = "mm", dpi = 900, device = cairo_pdf)


### Plot with scaling ----------------------------------------------------------
model.occupied <- summary(lm(log10(Occupied) ~ log10(POPESTIMATE), data = df_MSA))
lmtest::bptest(lm(log10(Occupied) ~ log10(POPESTIMATE), data = df_MSA))  # Breusch-Pagan test (p>>0.05 we cant reject the null of homoskedasticity)


### Plot
occupied_plot <- ggplot(df_MSA, aes(x = POPESTIMATE, y = Occupied)) +
  geom_point(size = 0.8, alpha = 0.7, color = "#7f9e5d") +
  geom_abline(intercept = model.occupied$coefficients[1, 1], 
              slope = model.occupied$coefficients[2, 1], 
              linewidth = 0.5, alpha = 1) + 
  geom_abline(#intercept = model.occupied$coefficients[1, 1], 
    intercept =  -0.34088,
    slope = 1, linetype = "dashed", 
    linewidth = 0.5, alpha = 0.6) +
  xlab("Population") + ylab("Occupied housing units") + 
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^4, 10^8), expand = c(0, 0)) + 
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^3, 10^7), expand = c(0, 0)) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.4, 0.4, 0.2, 0.2, "cm"), #t,r,b,l
    legend.position = "none",
    text = element_text(family = "Arial")
  )
occupied_plot 

#ggsave(filename = "Fig1_d.pdf", plot = occupied_plot, width = 60, height = 60, units = "mm", dpi = 900, device = cairo_pdf)








#-----------------------------------------------------------------------------------------------
### SI -----------------------------------------------------------------------------------------

# Use Census data instead ------------------------------------------------------
# Scaling for all MSAs - 2020 Census -------------------------------------------

# Remove NA and commas
Census2020 <- Census2020[!is.na(Census2020$Total), ]

Census2020$Total_Census <- as.numeric(gsub(",", "", Census2020$Total))
Census2020$Occupied_Census <- as.numeric(gsub(",", "", Census2020$Occupied))
Census2020$Vacant_Census <- as.numeric(gsub(",", "", Census2020$Vacant))

Census2020 <- Census2020 %>%
  select(-Total, -Occupied, -Vacant)

data_aux2 <- data[data$year == 2020, ]

df2020 <- merge(data_aux2, ACSD_data, 
            by.x = c("GeographicArea", "year"), 
            by.y = c("Label", "year"), 
            all.x = TRUE)

# Merge with df2020
census_df <- merge(df2020, Census2020, by.x = "GeographicArea", by.y = "Label", all =TRUE)


#check that all na counties are Alaska, Puerto Rico or Hawaii
na_geoid_df <- census_df[is.na(census_df$County_code), ]

census_df <- census_df[!is.na(census_df$County_code), ]

#Select only counties that are part of MSAs
dfMSACensus <- merge(census_df, MSA, by.x = "County_code", by.y = "County_Code", all.x = TRUE)
dfMSACensus <- dfMSACensus[!is.na(dfMSACensus$MAS_Code), ]



county_vac <- lm(log(Vacant_Census) ~ log(POPESTIMATE), data = census_df)
summary(county_vac)
c_vac <- lmtest::coeftest(county_vac, vcov = vcovHC(county_vac, type="HC2"))
ci_county_vac <- confint(c_vac , level = 0.95)



dfMSACensus <- dfMSACensus %>%
  group_by(MAS_Code) %>%
  summarize(
    Vacant_Census = sum(Vacant_Census, na.rm = TRUE),
    Occupied_Census = sum(Occupied_Census, na.rm = TRUE),
    Total_Census = sum(Total_Census, na.rm = TRUE),
    POPESTIMATE = sum(POPESTIMATE, na.rm = TRUE),
    .groups = "drop"
  )
dfMSACensus <- dfMSACensus[dfMSACensus$Vacant_Census > 0, ]

MSA_vac <- lm(log(Vacant_Census) ~ log(POPESTIMATE), data = dfMSACensus)
summary(MSA_vac)
MSA_vac <- lmtest::coeftest(MSA_vac, vcov = vcovHC(MSA_vac, type="HC2"))
ci_MSA_vac <- confint(MSA_vac , level = 0.95)




# Plots -------------------------------------------------------------------------

### Histogram % vacancy distribution ----
p_ACHS_vac_ratio <- ggplot(df_MSA, aes(x = ratio_vac)) +
  geom_histogram(bins = 30, fill = "#9b536d", color = "white", alpha = 0.8) +
  theme_classic(base_size = 8) +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    panel.grid = element_blank(),
    text = element_text(family = "Arial")
    )+
  xlab("Percentage of vacant housing units") + 
  ylab("Frequency")+
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 100, by = 10), expand = c(0, 0))
p_ACHS_vac_ratio


### Other histograms with alternative definitions of vacancy -------------------
### Histogram % non seasonal vacancy 
p_ACHS_vac_noSeasonal_ratio <- ggplot(df_MSA, aes(x = ratio_vac_noSeasonal)) +
  #geom_histogram(aes(y = ..density..), bins = 30, fill = "#9b536d", color = "white", alpha = 0.8) +
  geom_histogram(bins = 30, fill = "#470013", color = "white") +
  theme_classic(base_size = 8) +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    panel.grid = element_blank(),
    text = element_text(family = "Arial")
  )+
  xlab("Percentage of nonseasonal vacant housing units") + 
  ylab("Frequency") +
  scale_x_continuous(limits = c(0, 18), breaks = seq(0, 18, by = 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), expand = c(0, 0))
p_ACHS_vac_noSeasonal_ratio


### Histogram % other vacancy ACHS
p_ACHS_vac_other_ratio <- ggplot(df_MSA, aes(x = ratio_vac_other)) +
  #geom_histogram(aes(y = ..density..), bins = 30, fill = "#9b536d", color = "white", alpha = 0.8) +
  geom_histogram(bins = 30, fill = "darkred", color = "white") +
  theme_classic(base_size = 8) +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    panel.grid = element_blank(),
    text = element_text(family = "Arial")) +
  xlab("Percentage of \"other\" vacant housing units") + 
  ylab("Frequency") +
  scale_x_continuous(limits = c(0, 18), breaks = seq(0, 18, by = 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), expand = c(0, 0))
p_ACHS_vac_other_ratio


### Histogram % other vacancy USPS
p_USPS_vac_ratio <- ggplot(df_MSA, aes(x = ratio_vac_USPS)) +
  geom_histogram(bins = 30, fill = "#60088C", color = "white") +
  theme_classic(base_size = 8) +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    panel.grid = element_blank(),
    text = element_text(family = "Arial")) +
  xlab("Percentage of vacant residentail addresses") + 
  ylab("Frequency") +
  scale_x_continuous(limits = c(0, 18), breaks = seq(0, 18, by = 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), expand = c(0, 0))
p_USPS_vac_ratio 


### Deccenial Census 2020
dfMSACensus$ratio_vac_Census2020 <- (dfMSACensus$Vacant_Census / dfMSACensus$Total_Census) * 100

p_Census2020_vac_ratio <- ggplot(dfMSACensus , aes(x = ratio_vac_Census2020)) +
  geom_histogram(bins = 30, fill = "#5f2f41", color = "white") +
  theme_classic(base_size = 8) +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    panel.grid = element_blank(),
    text = element_text(family = "Arial")) +
  xlab("Percentage of vacant housing units") + 
  ylab("Frequency") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), expand = c(0, 0))
p_Census2020_vac_ratio


p <- (p_ACHS_vac_noSeasonal_ratio + p_ACHS_vac_other_ratio) / (p_USPS_vac_ratio + p_Census2020_vac_ratio) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 12, face = "bold"),
    theme(text = element_text(family = "Arial"))
  )
p

#ggsave(filename = "SI_Fig_22.pdf", plot = p, width = 180, height = 90, units = "mm", dpi = 900, device = cairo_pdf)



















### Plot of vacancy rates
custom_breaks <- c(3.6, 6.5, 11.9, 21.7, 39.4) 

p_ratio_vac <- ggplot() +
  geom_sf(data = df_MSA, aes(fill = ratio_vac), color = "grey60", linewidth = 0.2) +
  geom_sf(data = states_shp,fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_gradientn(
    colors = reds,
    name = "Percentage of vacant housing units",
    trans = "log",
    na.value = "white",
    breaks = custom_breaks,
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal",
      barwidth = 7,
      barheight = 0.4
    )
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    text = element_text(family = "Arial"),
    legend.position = c(0.15, 0.12),
    legend.justification = c("left", "bottom"),
    legend.title = element_text(size = 9, family = "Arial"),
    legend.text = element_text(size = 8, family = "Arial")
  )
p_ratio_vac


#ggsave(filename = "SI_Fig21_a.pdf", plot = p_ratio_vac, width = 120, height = 80, units = "mm", dpi = 900, device = cairo_pdf)









#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
### County equivalent analysis


# Data processing ----------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

# Remove Alaska, Puerto Rico and Hawaii
data <- subset(data, !grepl("Puerto Rico|Hawaii|Alaska", GeographicArea))

# Merge ACSD_df
data <- data[data$year == 2022, ]
df <- merge(data, ACSD_data, 
            by.x = c("GeographicArea", "year"), 
            by.y = c("Label", "year"), 
            all.x = TRUE)

# Merge with shapefile
df_county <- county_shp22 %>%
  left_join(df, by = c("GEOID" = "County_code"))

df_county <- df_county %>% filter(!is.na(POPESTIMATE))

df_county$perCapita_vac <- df_county$Vacant / df_county$POPESTIMATE
df_county$perCapita_occup <- df_county$Occupied / df_county$POPESTIMATE

df_county$ratio_vac <- (df_county$Vacant / df_county$Total) * 100
df_county$ratio_vac_noSeasonal <-  ((df_county$Vacant - df_county$For_seasonal_recreational_or_occasional_use) / df_county$Total) * 100
df_county$ratio_vac_other <-  (df_county$Other_vacant / df_county$Total) * 100

df_county$ratio_vac_USPS <-  (df_county$res_vac / df_county$ams_res) * 100


states_shp <- st_transform(states_shp, st_crs(df_county))

custom_breaks <- c(0.00784, 0.0420, 0.225, 1.209) 
# range_vals <- c(0.007838, 1.209)
# custom_breaksAUX <- exp(seq(log(range_vals[1]), log(range_vals[2]), length.out = 4))  # Get 4 evenly spaced breaks on the log scale
# custom_breaksAUX


# Plot
map_population <- ggplot() +
  geom_sf(data = df_county, aes(fill = perCapita_vac), color = NA) +
  geom_sf(data = states_shp, fill = NA, color = "black", size = 0.3) +
  scale_fill_gradientn(colors = reds, trans = "log", breaks = custom_breaks, name = "Vacant housing units per capita", na.value = "darkblue") +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    text = element_text(family = "Arial")
  )

map_fill_legend <- ggplot(df_county) +
  geom_sf(aes(geometry = geometry, fill = perCapita_vac), alpha = 0) +  # invisible layer
  scale_fill_gradientn(
    colors = reds,
    trans = "log",
    breaks = custom_breaks,
    name = "Vacant houses per capita",
    na.value = "darkgrey"
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 7,
      barheight = 0.4,
      title.position = "top",
      direction = "horizontal",
      label.theme = element_text(size = 6) 
    )
  ) +
  theme_void() +
  theme(text = element_text(family = "Arial"))

fill_legend <- get_legend(map_fill_legend)

final_p <- ggdraw() +
  draw_plot(map_population) +
  draw_plot(fill_legend, x = 0.01, y = 0.02, width = 0.45, height = 0.15, hjust = 0, vjust = 0)

final_p


#ggsave(filename = "SI_County_vacancy_map.pdf", plot = final_p, width = 120, height = 80, units = "mm", dpi = 900, device = cairo_pdf)


### Plot with scaling ----------------------------------------------------------
model.vacant <- summary(lm(log10(Vacant) ~ log10(POPESTIMATE), data = df_county))


### Plot
vacant_plot <- ggplot(df_county, aes(x = POPESTIMATE, y = Vacant)) +
  geom_point(size = 0.8, alpha = 0.7, color = "#9b536d") +
  geom_abline(intercept = model.vacant$coefficients[1, 1], 
              slope = model.vacant$coefficients[2, 1], 
              linewidth = 0.5, alpha = 1) +
  xlab("Population") + 
  ylab("Vacant housing units") + 
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^2, 10^8), expand = c(0, 0)) + 
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^1, 10^7), expand = c(0, 0)) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.4, 0.4, 0.2, 0.2, "cm"), #t,r,b,l
    legend.position = "none"
  )+
  theme(text = element_text(family = "Arial"))
vacant_plot

#ggsave(filename = "SI_Fig_2b.pdf", plot = vacant_plot, width = 60, height = 60, units = "mm", dpi = 900, device = cairo_pdf)




#-------------------------------------------------------------------------------
### Occupied Plots -------------------------------------------------------------

greens <- colorRampPalette(c("#e5ff9a", "#013220"))(50)

custom_breaks <- c(0.108, 0.218, 0.445, 0.909) 
# range_vals <- c(0.107, 0.909)
# custom_breaksAUX <- exp(seq(log(range_vals[1]), log(range_vals[2]), length.out = 4))  # Get 4 evenly spaced breaks on the log scale
# custom_breaksAUX

# Plot
map_population <- ggplot() +
  geom_sf(data = df_county, aes(fill = perCapita_occup), color = NA) +
  geom_sf(data = states_shp, fill = NA, color = "black", size = 0.3) +
  scale_fill_gradientn(colors = greens, trans = "log", breaks = custom_breaks, name = "Occupied housing units per capita", na.value = "darkblue") +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    text = element_text(family = "Arial")
  )

map_fill_legend <- ggplot(df_county) +
  geom_sf(aes(geometry = geometry, fill = perCapita_occup), alpha = 0) +  # invisible layer
  scale_fill_gradientn(
    colors = greens,
    trans = "log",
    breaks = custom_breaks,
    name = "Occupied housing units per capita",
    na.value = "darkgrey"
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 7,
      barheight = 0.4,
      title.position = "top",
      direction = "horizontal",
      label.theme = element_text(size = 6) 
    )
  ) +
  theme_void() +
  theme(text = element_text(family = "Arial"))

fill_legend <- get_legend(map_fill_legend)

final_p <- ggdraw() +
  draw_plot(map_population) +
  draw_plot(fill_legend, x = 0.01, y = 0.02, width = 0.45, height = 0.15, hjust = 0, vjust = 0)

final_p

#ggsave(filename = "SI_Fig_2c.pdf", plot = final_p, width = 120, height = 80, units = "mm", dpi = 900, device = cairo_pdf)


### Plot with scaling ----------------------------------------------------------
model.occupied <- summary(lm(log10(Occupied) ~ log10(POPESTIMATE), data = df_county))
lmtest::bptest(lm(log10(Occupied) ~ log10(POPESTIMATE), data = df_county))  # Breusch-Pagan test (p>>0.05 we cant reject the null of homoskedasticity)


### Plot
occupied_plot <- ggplot(df_county, aes(x = POPESTIMATE, y = Occupied)) +
  geom_point(size = 0.8, alpha = 0.7, color = "#7f9e5d") +
  geom_abline(intercept = model.occupied$coefficients[1, 1], 
              slope = model.occupied$coefficients[2, 1], 
              linewidth = 0.5, alpha = 1) +
  xlab("Population") + 
  ylab("Occupied housing units") + 
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^1, 10^7), expand = c(0, 0)) + 
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^1, 10^7), expand = c(0, 0)) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.4, 0.4, 0.2, 0.2, "cm"), #t,r,b,l
    legend.position = "none"
  )+
  theme(text = element_text(family = "Arial"))
occupied_plot 

#ggsave(filename = "SI_Fig2_d.pdf", plot = occupied_plot, width = 60, height = 60, units = "mm", dpi = 900, device = cairo_pdf)








