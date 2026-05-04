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

#ggsave(filename = "SI_Fig_12.pdf", plot = p_boxplot, width = 180, height = 80, units = "mm", dpi = 900, device = cairo_pdf)




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
#ggsave(filename = "SI_Fig_13.pdf", plot = p_rate_per_city, width = 180, height = 80, units = "mm", dpi = 900, device = cairo_pdf)





### ----------------------------------------------------------------------------
### Supplementary figures ------------------------------------------------------

df_t <- dfMSA 

### Fit cross-sectional scaling each year --------------------------------------
# log10(Vacant) = intercept + beta * log10(POPESTIMATE) so Y0 = 10^(intercept)

scale_year <- df_t %>%
  group_by(year) %>%
  do({
    fit <- lm(log10(Vacant) ~ log10(POPESTIMATE), data = .)
    
    tibble(
      intercept = coef(fit)[1],
      beta = coef(fit)[2],
      Y0 = 10^(coef(fit)[1]),
      r2 = summary(fit)$r.squared
    )
  }) %>%
  ungroup()



# Population sizes of 4 representative city sizes
pop_reps <- tibble(
  pop_group = factor(
    c("250,000", "500,000", "1,000,000", "2,500,000"),
    levels = c("250,000", "500,000", "1,000,000", "2,500,000")
  ),
  N_fixed = c(2.5e5, 5e5, 1e6, 2.5e6)
)


### Compute the trajectories from the scaling law ------------------------------
#V_hat = Y0 * N^beta
#frac_hat = V_hat / N

implied_traj <- expand_grid(
  year = sort(unique(scale_year$year)),
  pop_group = levels(pop_reps$pop_group)
) %>%
  left_join(pop_reps, by = "pop_group") %>%
  left_join(scale_year, by = "year") %>%
  mutate(
    V_hat = Y0 * (N_fixed^beta),
    frac_hat = V_hat / N_fixed,
    frac_hat_pct = 100 * frac_hat
  ) %>%
  arrange(pop_group, year)


### Plots ----------------------------------------------------------------------
pop_colors <- c(
  "250,000" = "#F4A261",
  "500,000" = "#8AB17D",
  "1,000,000" = "#2A9D8F",
  "2,500,000" = "#264653"
)


p_vac_evol <- ggplot(implied_traj, aes(x=year, y=V_hat, color=pop_group, group=pop_group)) +
  geom_point(size = 1) +
  scale_color_manual(values = pop_colors) +
  scale_x_continuous(breaks = sort(unique(implied_traj$year))) +
  scale_y_continuous(
    breaks = c(seq(8000, 90000, by = 20000), 110000), expand = c(0, 0), labels = scales::comma) +
  coord_cartesian(ylim = c(8000, 110000)) +
  labs(x = "Year",y = "Vacant housing units", color = "Population") +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), #t,r,b,l
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none",
    text = element_text(family = "Arial")
  )
p_vac_evol


p_perCapita_vac_evol  <- ggplot(implied_traj, aes(x=year, y=frac_hat, color=pop_group, group=pop_group)) +
  geom_point(size = 1) +
  scale_color_manual(values = pop_colors) +
  scale_x_continuous(breaks = sort(unique(implied_traj$year))) +
  scale_y_continuous(limits = c(0.03, 0.05), expand = c(0, 0)) +
  labs(x = "Year", y = "Per capita vacant housing units", color = "Population") +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), #t,r,b,l
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none",
    text = element_text(family = "Arial")
  )

p_evolution <- (p_vac_evol | p_perCapita_vac_evol) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "right",
    plot.tag = element_text(size = 12, face = "bold"),
    text = element_text(family = "Arial")
  )
p_evolution
#ggsave(filename = "SI_fig_9.pdf", plot = p_evolution, width = 180, height = 75, units="mm", dpi=900, device=cairo_pdf)




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















































