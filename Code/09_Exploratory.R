library(dplyr)
library(ggplot2)
library(extrafont)
library(readxl)
library(patchwork)
library(tidyr)
library(scales)

loadfonts(device = "win") 

setwd("")   # Path for working folder



#---------------------------------------------------------------------------------------------------------
#------------------------------------------------------------ DATA ---------------------------------------
### Load Data
data <- readRDS("Data/clean_data.rds")
ACSD_data <- readRDS("Data/ACSD house/ACSD_data_clean.rds")
Census2020 <- read_excel("Data/DECENNIALPL2020.H1-2025-03-26T211423.xlsx", sheet = 2)


# Crosswalks 
MSA <- read_excel("Data/qcew-county-msa-csa-crosswalk.xlsx", sheet = 3)



# Data processing ----------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
# Remove Alaska, Puerto Rico and Hawaii
data <- subset(data, !grepl("Puerto Rico|Hawaii|Alaska", GeographicArea))

# Merge ACSD_df
data <- data[data$year > 2009, ]

df <- merge(data, ACSD_data, 
            by.x = c("GeographicArea", "year"), 
            by.y = c("Label", "year"), 
            all.x = TRUE)

# Filter rows where the last three characters of `MSA Title` are "MSA"
MSA <- MSA[grep("MSA$", MSA$MAS_Title), ]

#Select only counties that are part of MSAs
dfMSA <- merge(df, MSA, by.x = "County_code", by.y = "County_Code", all.x = TRUE)
dfMSA <- dfMSA[!is.na(dfMSA$MAS_Code), ]

dfMSA <- dfMSA %>%
  group_by(year, MAS_Code) %>%   # group by year AND MSA
  summarize(
    Vacant = sum(Vacant, na.rm = TRUE),
    Occupied = sum(Occupied, na.rm = TRUE),
    Total = sum(Total, na.rm = TRUE),
    POPESTIMATE = sum(POPESTIMATE, na.rm = TRUE),
    .groups = "drop"
  )

#Sort by MAS_Code and year
dfMSA <- dfMSA %>% arrange(MAS_Code, year)




# Compute year-to-year rate of change
df_rate_Total <- dfMSA %>%
  group_by(MAS_Code) %>%
  arrange(year) %>%
  mutate(
    rate_change = (Total - lag(Total)) / lag(Total)
  ) %>%
  ungroup()

# Remove the first year per MAS_Code where lag is NA
df_rate_Total <- df_rate_Total %>% filter(!is.na(rate_change))



# Compute year-to-year rate of change
df_rate_Pop <- dfMSA %>%
  group_by(MAS_Code) %>%
  arrange(year) %>%
  mutate(
    rate_change = (POPESTIMATE - lag(POPESTIMATE)) / lag(POPESTIMATE)
  ) %>%
  ungroup()

# Remove the first year per MAS_Code where lag is NA
df_rate_Pop <- df_rate_Pop %>% filter(!is.na(rate_change))





# Compute year-to-year rate of change for Vacant housing
df_rate_vac <- dfMSA %>%
  group_by(MAS_Code) %>%
  arrange(year) %>%
  mutate(
    rate_change = (Vacant - lag(Vacant)) / lag(Vacant)
  ) %>%
  ungroup()

# Remove the first year per MAS_Code where lag is NA
df_rate_vac <- df_rate_vac %>% filter(!is.na(rate_change))



### ----------------------------------------------------------------------------
### Variation of total housing stock and population over time ------------------
 

## Boxplot of rate_change per year
p_total_change <- ggplot(df_rate_Total, aes(x = factor(year), y = rate_change)) +
  geom_boxplot(fill = "#8c510a", alpha=0.6, color = "black", outlier.alpha = 0.3, outlier.size = 0.5, linewidth = 0.4, median.linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.6) + 
  labs(x = "Year", y = "Annual rate of change in housing units", title = "") +
  scale_y_continuous(limits = c(-0.1, 0.1), expand = c(0, 0)) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), #t,r,b,l
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none",
    text = element_text(family = "Arial")
  )
#p_total_change


p_pop_change <- ggplot(df_rate_Pop, aes(x = factor(year), y = rate_change)) +
  geom_boxplot(fill = "#8c510a", alpha=0.6, color = "black", outlier.alpha = 0.3, outlier.size = 0.5, linewidth = 0.4, median.linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.6) +  
  labs(x = "Year", y = "Annual rate of change in population", title = "") +
  scale_y_continuous(limits = c(-0.1, 0.1), expand = c(0, 0)) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), #t,r,b,l
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none",
    text = element_text(family = "Arial")
  )
#p_pop_change


p_boxplot <- (p_total_change | p_pop_change) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 12, face = "bold"),
    theme(text = element_text(family = "Arial"))
  )
p_boxplot

#ggsave(filename = "SI_Fig_13.pdf", plot = p_boxplot, width = 180, height = 80, units = "mm", dpi = 900, device = cairo_pdf)




# Plot the change over time for each city --------------------------------------
# Palette
n <- length(unique(dfMSA$MAS_Code))
pal <- colorRamps::primary.colors(n)


p_rate_per_city_total <- df_rate_Total %>% filter(year >= 2011, year <= 2022) %>%
  ggplot(aes(x = year, y = rate_change, group = MAS_Code, color = MAS_Code)) +
  geom_line(alpha = 0.15, linewidth = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "black") +
  scale_color_manual(values = pal) +
  scale_y_continuous(limits = c(-0.1, 0.125), breaks = seq(-0.1, 0.125, by = 0.025), expand = c(0, 0)) +
  scale_x_continuous(breaks = 2011:2022) +
  labs(x = "Year", y = "Annual rate of change in housing units") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), # top, right, bottom, left
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(family = "Arial"),
    legend.position = "none"
  )
p_rate_per_city_total



p_rate_per_city_vac <- df_rate_vac %>% filter(year >= 2011, year <= 2022) %>%
  ggplot(aes(x = year, y = rate_change, group = MAS_Code, color = MAS_Code)) +
  geom_line(alpha = 0.15, linewidth = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "black") +
  scale_color_manual(values = pal) +
  scale_y_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.3, 0.3, by = 0.1), labels = scales::label_number(accuracy = 0.1), expand = c(0, 0)) +
  scale_x_continuous(breaks = 2011:2022) +
  labs(x = "Year", y = "Annual rate of change in housing units") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), # top, right, bottom, left
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(family = "Arial"),
    legend.position = "none"
  )
p_rate_per_city_vac



p_rate_per_city <- (p_rate_per_city_total | p_rate_per_city_vac) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 12, face = "bold"),
    theme(text = element_text(family = "Arial"))
  )
p_rate_per_city
#ggsave(filename = "SI_Fig_14.pdf", plot = p_rate_per_city, width = 180, height = 80, units = "mm", dpi = 900, device = cairo_pdf)






### Plots ----------------------------------------------------------------------
pop_colors <- c(
  "<160,000" = "#F4A261",
  "160,000-400,000" = "#8AB17D",
  "400,000-1,200,000" = "#2A9D8F",
  ">1,200,000" = "#264653"
)

# Create fixed 2020 population groups
msa_pop_2020 <- dfMSA %>%
  filter(year == 2010) %>%
  dplyr::select(MAS_Code, pop_2020 = POPESTIMATE) %>%
  distinct(MAS_Code, .keep_all = TRUE) %>%
  mutate(
    pop_group = case_when(
      pop_2020 < 160000 ~ "<160,000",
      pop_2020 >= 160000 & pop_2020 < 400000 ~ "160,000-400,000",
      pop_2020 >= 400000 & pop_2020 < 1200000 ~ "400,000-1,200,000",
      pop_2020 >= 1200000 ~ ">1,200,000"),
    pop_group = factor(pop_group, levels = names(pop_colors))
  )

# Add fixed 2020 groups back to all years
dfMSA2 <- dfMSA %>%
  left_join(msa_pop_2020, by = "MAS_Code") %>%
  mutate(perCapita_vac = Vacant / POPESTIMATE)


### Plot total Vaacant houses for each group per year

vacant_group_year <- dfMSA2 %>%
  group_by(year, pop_group) %>%
  summarise(
    total_vacant = sum(Vacant, na.rm = TRUE),
    .groups = "drop"
  )

vacant_group_year$log_total_vacant <- log(vacant_group_year$total_vacant)
p_total_vacant_groups <- ggplot(vacant_group_year, aes(x = year, y = log_total_vacant, color = pop_group, group = pop_group)) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(values = pop_colors, name = "Population group") +
  scale_x_continuous(breaks = sort(unique(vacant_group_year$year))) +
  scale_y_continuous(limits = c(13.5, 16), expand = expansion(mult = c(0, 0))) +
  labs(x = "Year", y = "ln(Total vacant housing units)") +
  theme_classic(base_size = 8) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    text = element_text(family = "Arial"),
    legend.position = "none"
  )

p_total_vacant_groups


### ----------------------------------------------------------------------------
### Plot of Total population vs. year (circle size indicate the per capita) ----

pop_group_year <- dfMSA2 %>%
  group_by(year, pop_group) %>%
  summarise(
    total_population = sum(POPESTIMATE, na.rm = TRUE),
    total_vacant = sum(Vacant, na.rm = TRUE),
    perCapita_vac = total_vacant / total_population,
    .groups = "drop")

pop_group_year$log_total_population <- log(pop_group_year$total_population)

p_total_pop_groups <- ggplot(pop_group_year, aes(x = year, y = log_total_population, group = pop_group)) +
  geom_line(aes(color = pop_group), linewidth = 0.2) +
  geom_point(aes(size = perCapita_vac, fill = pop_group), shape = 21, color = "black", stroke = 0.2, alpha = 0.9) +
  scale_color_manual(values = pop_colors, name = "Population group", drop = FALSE) +
  scale_fill_manual(values = pop_colors, name = "Population group", drop = FALSE) +
  scale_size_continuous(name = "Vacant units per capita", range = c(0.1, 3.5)) +
  scale_y_continuous(limits = c(16, 19), expand = expansion(c(0, 0.0))) +
  scale_x_continuous(breaks = sort(unique(pop_group_year$year))) +
  labs(x = "Year", y = "ln(Total population)") +
  theme_classic(base_size = 8) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    text = element_text(family = "Arial"),
    legend.position = "none"
  )
p_total_pop_groups


p <- (p_total_vacant_groups | p_total_pop_groups) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 10, face = "bold"),
    text = element_text(family = "Arial"))
p

#ggsave(filename = "Fig2_b.pdf", plot = p_total_pop_groups, width = 70, height = 60, units="mm", dpi=900, device=cairo_pdf)







### ----------------------------------------------------------------------------
### Supplementary figures ------------------------------------------------------

### Plot of total and per capita vacant housing units 2010-2022 ----------------

# Aggregate by year
df_year <- dfMSA %>%
  group_by(year) %>%
  summarise(
    total_vacant = sum(Vacant, na.rm = TRUE),
    total_population = sum(POPESTIMATE, na.rm = TRUE),
    vacant_per_capita = total_vacant / total_population
  ) %>%
  ungroup()

# Plot total vacant houses
p_total_vac <- ggplot(df_year, aes(x = year, y = total_vacant)) +
  geom_line(linewidth = 0.7, color = "#9b536d") +
  geom_point(size = 1, color = "#9b536d") +
  scale_x_continuous(breaks = 2010:2022) +
  scale_y_continuous(limits = c(10500000, 12000000), breaks = seq(10500000, 12000000, by = 500000), labels = comma, expand = c(0, 0)) +
  labs(x = "Year", y = "Vacant housing units") +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), #t,r,b,l
    axis.text.x = element_text(angle = 45, hjust = 1), 
    text = element_text(family = "Arial")
  )


# Plot total vacant houses per capita
p_total_vac_perCapita  <- ggplot(df_year, aes(x = year, y = vacant_per_capita)) +
  geom_line(linewidth = 0.7, color = "#9b536d") +
  geom_point(size = 1, color = "#9b536d") +
  scale_x_continuous(breaks = 2010:2022) +
  scale_y_continuous(limits = c(0.036, 0.048), breaks = seq(0.036, 0.048, by = 0.002), expand = c(0, 0)) +
  labs(x = "Year", y = "Per capita vacant housing units") +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), #t,r,b,l
    axis.text.x = element_text(angle = 45, hjust = 1), 
    text = element_text(family = "Arial")
  )


p <- (p_total_vac | p_total_vac_perCapita) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "right",
    plot.tag = element_text(size = 12, face = "bold"),
    text = element_text(family = "Arial")
  )
p
#ggsave(filename = "SI_fig_5.pdf", plot = p, width = 180, height = 70, units="mm", dpi=900, device=cairo_pdf)















































