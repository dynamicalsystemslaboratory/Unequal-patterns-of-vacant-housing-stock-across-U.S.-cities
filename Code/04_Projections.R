#----------------------------------------------------------
# RUN Parameter_extrapolation.R before running this script

setwd("")  
source("Code/Parameter_extrapolation.R")
#----------------------------------------------------------

library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)
library(extrafont)
library(Cairo)
library(ggalluvial)
library(tidyverse)
library(patchwork)

loadfonts(device = "win") 
#fonts()


#---------------------------------------------------------------------------------------------------------
#------------------------------------------------------------ DATA ---------------------------------------

# Taken from: "Hauer, M. E. (2019). Population projections for US counties by age, sex, and race controlled to 
# shared socioeconomic pathway. Scientific data, 6(1), 1-15".
fr <- fread("Data/SSP_asrc/SSP_asrc.csv", nThread = parallel::detectCores(), showProgress = TRUE)
pop_projections <- as.data.frame(fr) 

ACSD_data <- readRDS("Data/ACSD house/ACSD_data_clean.rds")

MSA <- read_excel("Data/qcew-county-msa-csa-crosswalk.xlsx", sheet = 3)




# Data processing ----------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
# Get aggregated pop_projections per county until 2050 --------
pop_totals <- pop_projections %>%
  group_by(GEOID, YEAR) %>%
  summarise(
    SSP1 = sum(SSP1, na.rm = TRUE),
    SSP2 = sum(SSP2, na.rm = TRUE),
    SSP3 = sum(SSP3, na.rm = TRUE),
    SSP4 = sum(SSP4, na.rm = TRUE),
    SSP5 = sum(SSP5, na.rm = TRUE),
    .groups = "drop"
  )

pop_totals <- pop_totals[pop_totals$YEAR == 2050, ]

# Merge pop_totals with dfMSA
dfMSA$County_code <- as.numeric(dfMSA$County_code) #only has counties that are part of MSAs
dfMSA_aux <- dfMSA[dfMSA$year == 2022, ]

projections_df <- merge(x = dfMSA_aux, y = pop_totals, 
                        by.x = "County_code", by.y = "GEOID", 
                        all.x = TRUE
)

na_rows <- projections_df[is.na(projections_df$SSP2), ]

projections_df <- projections_df %>%
  group_by(MAS_Code) %>%
  summarize(
    Vacant = sum(Vacant, na.rm=TRUE),
    Occupied = sum(Occupied, na.rm=TRUE),
    Total = sum(Total, na.rm=TRUE),
    POPESTIMATE = sum(POPESTIMATE, na.rm=TRUE),
    SSP1 = sum(SSP1, na.rm=TRUE),
    SSP2 = sum(SSP2, na.rm=TRUE),
    SSP3 = sum(SSP3, na.rm=TRUE),
    SSP4 = sum(SSP4, na.rm=TRUE),
    SSP5 = sum(SSP5, na.rm=TRUE),
    .groups = "drop"
  )

#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

# Get slope and intercept projections (run "Parameter_extrapolation.R")
Beta_2050_linear_val <- Beta_pred_2050_linear_val
Beta_2050_spline_val <- Beta_pred_2050_spline_val
Alpha_2050_linear_val <- Alpha_pred_2050_linear_val 
Alpha_2050_spline_val <- Alpha_pred_2050_spline_val



#-------------------------------------------------------------------------------------------------------------
# Get the projection of vacant houses in 2050, Vacant_2050
# 1) Using Beta_linear & Alpha_linear
# 2) Using Beta_linear & Alpha_spline
# 3) Using Beta_spline & Alpha_linear
# 4) Using Beta_spline & Alpha_spline


#Assuming SSP2 (Middle of the Road) projections from Hauer, M. E. (2019)
projections_df$Vacant_2050_1 <- (exp(Alpha_2050_linear_val)) * (projections_df$SSP2 ^ Beta_2050_linear_val)
#projections_df$Vacant_2050_2 <- (exp(Alpha_2050_spline_val)) * (projections_df$SSP2 ^ Beta_2050_linear_val)
#projections_df$Vacant_2050_3 <- (exp(Alpha_2050_linear_val)) * (projections_df$SSP2 ^ Beta_2050_spline_val)
#projections_df$Vacant_2050_4 <- (exp(Alpha_2050_spline_val)) * (projections_df$SSP2 ^ Beta_2050_spline_val)

projections_df$Vacant_2050_1_perCapita <- projections_df$Vacant_2050_1 / projections_df$SSP2
#projections_df$Vacant_2050_2_perCapita <- projections_df$Vacant_2050_2 / projections_df$SSP2
#projections_df$Vacant_2050_3_perCapita <- projections_df$Vacant_2050_3 / projections_df$SSP2
#projections_df$Vacant_2050_4_perCapita <- projections_df$Vacant_2050_4 / projections_df$SSP2
#-------------------------------------------------------------------------------------------------------------

projections_df$Vacant_2022_perCapita <- projections_df$Vacant / projections_df$POPESTIMATE


# Threshold for "No trend" (5% = 0.05)
no_trend_rel_threshold <- 0


plot_input <- projections_df %>%
  mutate(
    POPESTIMATE = as.numeric(POPESTIMATE),
    Vacant = as.numeric(Vacant),
    future_vacant_pc = as.numeric(Vacant_2050_1_perCapita),
    current_vacant_pc = ifelse(POPESTIMATE == 0, NA_real_, Vacant / POPESTIMATE),
    rel_change = case_when(
      is.na(current_vacant_pc) & future_vacant_pc == 0 ~ 0,
      !is.na(current_vacant_pc) ~ (future_vacant_pc - current_vacant_pc) / current_vacant_pc,
      TRUE ~ NA_real_
    ),
    vacancy_trend = case_when(
      is.infinite(rel_change) & future_vacant_pc > 0 ~ "Increasing",
      rel_change > no_trend_rel_threshold ~ "Increasing",
      rel_change < -no_trend_rel_threshold ~ "Decreasing",
      TRUE ~ "No trend"
    ),
    pop_cat = case_when(
      POPESTIMATE >= 1e6 ~ "L",
      POPESTIMATE >= 5e5 ~ "M",
      TRUE ~ "S"
    ),
    future_bin = dplyr::ntile(future_vacant_pc, 5)
  ) %>%
  select(MAS_Code, pop_cat, vacancy_trend, future_bin, future_vacant_pc) %>%
  filter(vacancy_trend != "No trend")

# Compute min/max per bin for labels
bin_ranges <- plot_input %>%
  group_by(future_bin) %>%
  summarise(
    min_val = min(future_vacant_pc, na.rm = TRUE),
    max_val = max(future_vacant_pc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # create labels, make unique if duplicated
    label = paste0(sprintf("%.3f", min_val), " - ", sprintf("%.3f", max_val)),
    label = make.unique(label, sep = " ")
  )

# Aggregate counts for alluvial plot and attach bin labels
alluv_df <- plot_input %>%
  group_by(pop_cat, vacancy_trend, future_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(bin_ranges %>% select(future_bin, label), by = "future_bin") %>%
  mutate(
    vacancy_trend = factor(vacancy_trend, levels = c("Decreasing", "Increasing")),
    pop_cat = factor(pop_cat, levels = c("L", "M", "S")),
    future_bin = factor(label, levels = unique(label)) 
  )



# Aggregate to counts by combination (y = number of MSAs flowing) --------------

alluv_df <- plot_input %>%
  group_by(pop_cat, vacancy_trend) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    # order factors so the plot shows them correctly
    pop_cat = factor(pop_cat, levels = c("L", "M", "S")),
    vacancy_trend = factor(vacancy_trend, levels = c("Increasing", "No trend", "Decreasing"))
  )

# Make sure population categories are ordered
alluv_df <- alluv_df %>%
  mutate(
    vacancy_trend = factor(vacancy_trend, levels = c("Increasing", "No trend", "Decreasing")),
    pop_cat = factor(pop_cat, levels = c("L", "M", "S"))
  )

# Colors for each population size
pop_colors <- c(
  "S" = "#F4A261",
  "M" = "#2A9D8F",
  "L" = "#264653"
)


### Plot horizontal bar chart
p_barplot <- ggplot(alluv_df, aes(x = n, y = vacancy_trend, fill = pop_cat)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Number of cities", y = "Future vacancy trends 2050", fill = "City size") +
  theme_minimal() +
  scale_fill_manual(values = pop_colors) +
  scale_x_reverse(breaks = seq(0, 200, by = 50), limits = c(0, 200), expand = c(0,0)) +
  theme_linedraw() +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    #panel.grid = element_blank(),
    legend.position = c(0.05, 0.95),    
    legend.justification = c("left", "top"),
    text = element_text(family = "Arial"),
    axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  ) 

p_barplot 


## ----------------------------------------

bin_ranges <- plot_input %>%
  group_by(future_bin) %>%
  summarise(
    min_val = min(future_vacant_pc, na.rm = TRUE),
    max_val = max(future_vacant_pc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label = paste0(sprintf("%.3f", min_val), " - ", sprintf("%.3f", max_val)))

# Aggregate counts
alluv_df <- plot_input %>%
  group_by(pop_cat, vacancy_trend, future_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(bin_ranges %>% select(future_bin, label), by = "future_bin") %>%
  mutate(
    node_left = paste0(pop_cat, "_", vacancy_trend),
    node_right = label
  )

# Order
desired_order <- c("S_Decreasing", "M_Decreasing", "L_Decreasing", "S_Increasing", "M_Increasing", "L_Increasing")
alluv_df$node_left <- factor(alluv_df$node_left, levels = desired_order)


alluv_df$fill <- pop_colors[alluv_df$pop_cat]

df <- alluv_df

# Add a dummy block to create a gap
dummy <- data.frame(
  node_left = "gap_block",
  node_right = NA,
  n = 15, # height of the gap
  fill= "white"
)
df2 <- bind_rows(df, dummy)

# Make the axis factors in desired order
df2$node_left  <- factor(df2$node_left, 
                         levels = c("S_Decreasing","M_Decreasing","L_Decreasing",
                                    "gap_block",
                                    "S_Increasing","M_Increasing","L_Increasing"))
df2$node_right <- factor(df2$node_right, levels = c(unique(df$node_right)))  # keep original right order

### Plot
p_alluv_1 <- ggplot(df2, aes(axis1 = node_left, axis2 = node_right, y = n)) +
  geom_alluvium(aes(fill = fill), width = 1/25, alpha = 0.9) +
  geom_stratum(fill = "white", color = "black", width = 1/25, linewidth = 0.3) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_fill_identity() +
  theme_minimal(base_size = 10) +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    text = element_text(family = "Arial")
  ) +
  labs(y = NULL, x = NULL)

p_alluv_1


p_predictions <- (p_barplot | p_alluv_1) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 12, face = "bold"),
    theme(text = element_text(family = "Arial"))
  )
p_predictions
#ggsave(filename = "Figures/1130_Main_predictions.pdf", plot = p_predictions, width = 180, height = 70, units = "mm", dpi = 900, device = cairo_pdf)



### To run the alternatice scenarios just change the combination of parameters







