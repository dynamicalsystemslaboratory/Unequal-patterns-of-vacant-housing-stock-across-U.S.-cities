library(dplyr)
library(sf)
library(ggplot2)
library(readxl)
library(patchwork)
library(extrafont)
library(Cairo)
library(sandwich)
library(splines)
library(tidyverse)

loadfonts(device = "win") 
fonts()

setwd("") # Path for working folder


#---------------------------------------------------------------------------------------------------------
#------------------------------------------------------------ DATA ---------------------------------------
data <- readRDS("Data/clean_data.rds")
ACSD_data <- readRDS("Data/ACSD house/ACSD_data_clean.rds")

# Crosswalks 
MSA <- read_excel("Data/qcew-county-msa-csa-crosswalk.xlsx", sheet = 3)

#---------------------------------------------------------------------------------------------------------
#------------------------------- MISC --------------------------------------------------------------------

### Auxiliar functions to plot longitudinal trajectories of MSAs that exhibit single power law
make_axis <- function(x, n_breaks = 5, pad_frac = 0.08) {
  x <- x[is.finite(x)]
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  x_range <- x_max - x_min
  
  if (x_range == 0) {
    x_range <- abs(x_min)
    if (x_range == 0) x_range <- 1
  }
  raw_min <- x_min - pad_frac * x_range
  raw_max <- x_max + pad_frac * x_range
  brks <- pretty(c(raw_min, raw_max), n = n_breaks)
  step <- brks[2] - brks[1]
  
  lower <- floor(raw_min / step) * step
  upper <- ceiling(raw_max / step) * step
  brks_final <- seq(lower, upper, by = step)
  
  list(
    limits = c(lower, upper),
    breaks = brks_final
  )
}

make_powerlaw_plot <- function(code, n_breaks = 5, x_pad = 0.04, y_pad = 0.08) {
  df_tmp <- dfMSA %>% 
    filter(MAS_Code == code) %>%
    filter(
      is.finite(POPESTIMATE),
      is.finite(Vacant),
      POPESTIMATE > 0,
      Vacant > 0
    )
  
  # Fit power law
  m <- lm(log10(Vacant) ~ log10(POPESTIMATE), data = df_tmp)
  beta <- coef(m)[2]
  a <- 10^(coef(m)[1])
  
  x_axis <- make_axis(df_tmp$POPESTIMATE, n_breaks = n_breaks, pad_frac = x_pad)
  y_axis <- make_axis(df_tmp$Vacant, n_breaks = n_breaks, pad_frac = y_pad)
  
  xgrid <- seq(min(df_tmp$POPESTIMATE, na.rm = TRUE), max(df_tmp$POPESTIMATE, na.rm = TRUE), length.out = 200)
  pred_df <- data.frame(POPESTIMATE = xgrid, Vacant = a * xgrid^beta)
  
  x_text <- x_axis$limits[1] + 0.04 * diff(x_axis$limits)
  y_text <- y_axis$limits[2] - 0.08 * diff(y_axis$limits)
  
  ggplot(df_tmp, aes(x = POPESTIMATE, y = Vacant)) +
    geom_point(aes(color = year), size = 1.5) +
    geom_line(data = pred_df, aes(x = POPESTIMATE, y = Vacant), inherit.aes = FALSE, color = "black", linewidth = 0.3) +
    scale_color_viridis_c(option = "viridis", direction = -1) +
    scale_x_continuous(limits = x_axis$limits, breaks = x_axis$breaks, labels = scales::comma, expand = expansion(mult = 0)) +
    scale_y_continuous(limits = y_axis$limits, breaks = y_axis$breaks, labels = scales::comma, expand = expansion(mult = 0)) +
    coord_cartesian(xlim = x_axis$limits, ylim = y_axis$limits, clip = "off") +
    annotate("text", x = x_text, y = y_text,
             label = paste0(
               "β = ", sprintf("%.3f", beta),
               "\nR² = ", sprintf("%.3f", summary(m)$r.squared)),
             hjust = 0, vjust = 1, size = 3) +
    labs(title = paste0(unique(df_tmp$MAS_Title), " (", code, ")"), x = "Population", y = "Vacant housing units",color = "Year") +
    theme_bw(base_size = 9) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 9),
      plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
      text = element_text(family = "Arial"),
      legend.position = c(0.85, 0.45),
      legend.key.size = unit(0.2, "cm"),
      legend.background = element_blank()
    )
}




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
    MAS_Title = first( MAS_Title),
    Vacant = sum(Vacant, na.rm = TRUE),
    Occupied = sum(Occupied, na.rm = TRUE),
    Total = sum(Total, na.rm = TRUE),
    POPESTIMATE = sum(POPESTIMATE, na.rm = TRUE),
    .groups = "drop"
  )






# ------------------------------------------------------------------------------
### Analysis -------------------------------------------------------------------

### Longitudinal scaling for all cities

# Palette
n <- length(unique(dfMSA$MAS_Code))
pal <- colorRamps::primary.colors(n)


### Inset plot of longitudinal trajectory of Denver, CO, MSA "C1974" -----------
code <- "C1974"
df_tmp <- dfMSA %>% filter(MAS_Code == code)

m <- lm(log10(Vacant) ~ log10(POPESTIMATE), data = df_tmp)
beta <- coef(m)[2]
a <- 10^(coef(m)[1])

# prediction grid on the original x scale
xgrid <- seq(min(df_tmp$POPESTIMATE, na.rm = TRUE), max(df_tmp$POPESTIMATE, na.rm = TRUE), length.out = 200)

pred_df <- data.frame(
  POPESTIMATE = xgrid,
  Vacant = a * xgrid^beta
)

p <- ggplot(df_tmp, aes(x = POPESTIMATE, y = Vacant)) +
  geom_point(color = "darkblue", size = 1) +
  geom_line(data = pred_df, aes(x = POPESTIMATE, y = Vacant), color = "black", linewidth = 0.2) +
  annotate( "text", x = min(df_tmp$POPESTIMATE, na.rm = TRUE), y = max(df_tmp$Vacant, na.rm = TRUE) * 0.9,
            label = paste0(
              "β = ", sprintf("%.3f", beta),
              "\nR² = ", sprintf("%.3f", summary(m)$r.squared)),
            hjust = 0, vjust = 1, size = 1) +
  labs(title = paste0(unique(df_tmp$MAS_Title), " (", code, ")"), x = "Population", y = "Vacant housing units") +
  theme_bw(base_size = 5) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 2),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    text = element_text(family = "Arial"),
    legend.position = "none"
  )
p


#ggsave(filename = "G:/My Drive/NatCities review/[rev] Longitudinal scaling_inset_a.pdf", plot=p, width=36, height=26, units="mm", dpi=900, device=cairo_pdf)




### Longitudinal scaling for all MSAs with Denver, CO in blue ------------------
code <- "C1974"
df_tmp <- dfMSA %>% filter(MAS_Code == code)


### Inset plot
m <- lm(log10(Vacant) ~ log10(POPESTIMATE), data = df_tmp)
beta <- coef(m)[2]
a <- 10^(coef(m)[1])

# prediction grid on the original x-scale
xgrid <- seq(min(df_tmp$POPESTIMATE, na.rm = TRUE), max(df_tmp$POPESTIMATE, na.rm = TRUE), length.out = 200)

pred_df <- data.frame(
  POPESTIMATE = xgrid,
  Vacant = a * xgrid^beta
)

p <- ggplot(df_tmp, aes(x = POPESTIMATE, y = Vacant)) +
  geom_point(color = "darkblue", size = 1) +
  geom_line(data = pred_df, aes(x = POPESTIMATE, y = Vacant), color = "black", linewidth = 0.2) +
  annotate( "text", x = min(df_tmp$POPESTIMATE, na.rm = TRUE), y = max(df_tmp$Vacant, na.rm = TRUE) * 0.9,
            label = paste0(
              "β = ", sprintf("%.3f", beta),
              "\nR² = ", sprintf("%.3f", summary(m)$r.squared)),
            hjust = 0, vjust = 1, size = 1) +
  labs(title = paste0(unique(df_tmp$MAS_Title), " (", code, ")"), x = "Population", y = "Vacant housing units") +
  theme_bw(base_size = 5) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 2),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    text = element_text(family = "Arial"),
    legend.position = "none"
  )
p

#ggsave(filename = "G:/My Drive/NatCities review/[rev] Longitudinal scaling_inset_a.pdf", plot=p, width=36, height=26, units="mm", dpi=900, device=cairo_pdf)



# Global scaling law using al the MSAs 2010-2022
city_lm <- lm(log10(Vacant) ~ log10(POPESTIMATE), data = df_tmp)
summary(city_lm)
confint(city_lm, level = 0.95)
coeftest(city_lm, vcov = vcovHC(city_lm, type = "HC2"))



#----------------------------------------------------------
#----------------------------------------------------------
### Here we visually inspect the trajectories of all MSAs and classify them according 
### to their trajectories. If they are well described by a single power law they are Type 1
### If they seem to follow 2 power laws - Type 2
### other


### Observations regarding R2 ranges:
# 0.8 - 1: Type 1
# Below 0.8 it starts to get a bit dubious. We'll consider Type 1 R2 > 0.7







### ----------------------------------------------------------------------------
### Function that shows plots according to the selected R2 range ---------------


### Function to plot the trajectory over time for a specific city --------------
plot_city <- function(code) {
  ggplot(dfMSA %>% filter(MAS_Code == code),
         aes(x = POPESTIMATE, y = Vacant)) +
    geom_point(aes(color = year), size = 3) +
    scale_x_log10() + scale_y_log10() +
    scale_color_viridis_c(option = "viridis", direction = -1) +
    annotate("text",
             x = min(dfMSA$POPESTIMATE[dfMSA$MAS_Code == code]) * 1, y = max(dfMSA$Vacant[dfMSA$MAS_Code == code]) * 0.9,
             label = {
               df_tmp <- dfMSA %>% filter(MAS_Code == code)
               m <- lm(log10(Vacant) ~ log10(POPESTIMATE), data = df_tmp)
               paste0("β = ", sprintf("%.3f", coef(m)[2]),"\nR² = ", sprintf("%.3f", summary(m)$r.squared))},
             hjust = 0, vjust = 1, size = 4) +
    labs(title = paste0(unique(df_tmp$MAS_Title), " (", code, ")"), x = "Population", y = "Vacant housing units", color = "Year") +
    theme_classic(base_size = 12) +
    theme(text = element_text(family = "Arial"))}




### Function to plot MSAs whose longitudinal vacancy vs. population power law fit falls within the specified R2
plot_by_r2 <- function(min_R2, max_R2, df = dfMSA) {
  r2_table <- df %>%
    filter(Vacant > 0, POPESTIMATE > 0) %>%
    group_by(MAS_Code, MAS_Title) %>%
    summarise({
      m <- lm(log10(Vacant) ~ log10(POPESTIMATE), data = cur_data())
      s <- summary(m)$coefficients
      
      tibble(
        R2 = summary(m)$r.squared,
        temporal_slope = s["log10(POPESTIMATE)", "Estimate"],
        standard_error = s["log10(POPESTIMATE)", "Std. Error"]
      )
    }, .groups = "drop")
  
  selected_cities <- r2_table %>%
    filter(R2 >= min_R2, R2 <= max_R2) %>%
    arrange(R2)
  print(selected_cities)
  
  plots <- map(selected_cities$MAS_Code, plot_city)
  
  return(plots)
}



# Run the function plot_by_r2
plot_by_r2(0.7, 1.0) # Assume all Type 1


# Save cities with R2>0.7 as Type 1 and set Type 2 -----------------------------

# Type 2 codes 
type2_codes <- c(
  "C2522","C4014","C2866","C1598","C1346","C1390","C1110","C3915","C1154","C2010","C3118","C3493", # 0.6 < R2 <= 0.7
  "C2222","C4614","C4868","C3798","C2734","C1338","C3566","C2458","C4266","C2518",
  "C2982","C4418","C4550","C2430","C3098","C3938","C1474","C1374","C2894","C1078",
  "C2810","C3078","C4442","C2242","C3458","C4590","C3310","C4166","C1766","C3406",
  "C3650","C2166","C2698","C4114","C1870","C2974","C2050","C4758","C1298","C1458",
  "C4268","C2178","C4378","C3086","C2954","C4942","C3258","C2390","C3854","C2542",
  "C4406","C4142","C4110","C2888","C2550","C1242","C1402","C3278","C4962","C1018",
  "C3974","C2842","C2074","C2214","C4726","C4214","C3886","C1654","C2506","C1730",
  "C3046","C1682","C2354","C2574","C4830","C1686")

# Compute R2 and slope
r2_table <- dfMSA %>%
  group_by(MAS_Code, MAS_Title) %>%
  summarise({
    m <- lm(log10(Vacant) ~ log10(POPESTIMATE), data = cur_data())
    s <- summary(m)$coefficients
    
    tibble(
      R2 = summary(m)$r.squared,
      temporal_slope = s["log10(POPESTIMATE)", "Estimate"],
      standard_error = s["log10(POPESTIMATE)", "Std. Error"]
    )
  }, .groups = "drop")


dfMSA_with_type <- dfMSA %>%
  left_join(r2_table, by = c("MAS_Code", "MAS_Title")) %>%
  mutate(Type = case_when(
    MAS_Code %in% type2_codes ~ "Type 2",
    R2 > 0.7 ~ "Type 1", TRUE ~ "Other"))


# Inspect the results for Type 1
sum(dfMSA_with_type$Type == "Type 1")/13 #There are 13 years

dfMSA_Type1 <- dfMSA_with_type %>%
  filter(Type == "Type 1") %>%
  distinct(MAS_Code, MAS_Title, .keep_all = TRUE)


## Save a csv with the identified Type 1, Type 2 and "other" MSAs
#write.csv(dfMSA_with_type, "Data/dfMSA_with_type.csv", row.names = FALSE) #Save

#----------------------------------------------------------
#----------------------------------------------------------






### -----------------------------------------------------------------------------
### Plot longitudinal trajectories of selected Type 1 MSAs that show single power law

p_C1222 <- make_powerlaw_plot("C1222", n_breaks = 4, x_pad = 0.02, y_pad = 0.04)
p_C3370 <- make_powerlaw_plot("C3370", n_breaks = 4, x_pad = 0.02, y_pad = 0.04)
p_C4098 <- make_powerlaw_plot("C4098", n_breaks = 4, x_pad = 0.02, y_pad = 0.06)
p_C2778 <- make_powerlaw_plot("C2778", n_breaks = 4, x_pad = 0.02, y_pad = 0.04)

p_t1 <- (p_C1222 | p_C2778) / (p_C3370 | p_C4098) +
  plot_annotation(tag_levels = "a")

p_t1 <- p_t1 &
  theme(
    plot.tag = element_text(size = 12, face = "bold"),
    text = element_text(family = "Arial")
  )
p_t1


### Plot of Type 2 MSA that resembles two power laws ---------------------------
code <- "C2458"
df_tmp <- dfMSA %>% 
  filter(MAS_Code == code)

# range
x_min <- min(df_tmp$POPESTIMATE, na.rm = TRUE)
x_max <- max(df_tmp$POPESTIMATE, na.rm = TRUE)
y_min <- min(df_tmp$Vacant, na.rm = TRUE)
y_max <- max(df_tmp$Vacant, na.rm = TRUE)
x_pad <- 0.04 * (x_max - x_min)
y_pad <- 0.08 * (y_max - y_min)

x_limits <- c(x_min - x_pad, x_max + x_pad)
y_limits <- c(y_min - y_pad, y_max + y_pad)

x_breaks <- seq(x_limits[1], x_limits[2], length.out = 5)
y_breaks <- seq(y_limits[1], y_limits[2], length.out = 5)

p_C2458 <- ggplot(df_tmp, aes(x = POPESTIMATE, y = Vacant)) +
  geom_point(aes(color = year), size = 1.5) +
  scale_x_continuous(limits = x_limits, breaks = x_breaks, labels = scales::comma, expand = c(0, 0)) +
  scale_y_continuous(limits = y_limits, breaks = y_breaks, labels = scales::comma, expand = c(0, 0)) +
  scale_color_viridis_c(option = "viridis", direction = -1) +
  labs(title = paste0(unique(df_tmp$MAS_Title), " (", code, ")"), x = "Population", y = "Vacant housing units", color = "Year") +
  theme_bw(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 9),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    text = element_text(family = "Arial"),
    legend.position = c(0.85, 0.45),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank()
  )
p_C2458

#ggsave(filename = "SI_Fig_10.pdf", plot=p_C2458, width=70, height=70, units="mm", dpi=900, device=cairo_pdf)








#----------------------------------------------------------
### Run 06_Extra_robustness.R before running this part script

setwd("")  
source("Code/06_Extra_robustness.R")
#----------------------------------------------------------


### ----------------------------------------------------------------------------
### Quadrant plot with the distribution of Type 1 betas ------------------------

# -Get Type 1 MSAs and plot them in a quadrant plot (y-axis: beta value; x-axis: % Population change from 2010-2022)
# -Population change is defined as the % change of the last year compared to the first year


### Read dfMSA_with_type
dfMSA_with_type <- read.csv("Data/dfMSA_with_type.csv") # List of MSAs and their types after visual inspection
dfMSA_with_type <- dfMSA_with_type[dfMSA_with_type$Type == "Type 1", ]


# We select only the MSAs that R^2 >=0.7 after fitting a single power law (88 in total)
dfMSA_with_type <- dfMSA_with_type[dfMSA_with_type$R2 >= 0.7, ] 


df_quad <- dfMSA_with_type %>%
  arrange(MAS_Code, year) %>%
  group_by(MAS_Code, MAS_Title, Type) %>%
  summarise(
    pop_first = first(POPESTIMATE),
    pop_last  = last(POPESTIMATE),
    pop_change_pct = 100 * (pop_last - pop_first) / pop_first,
    temporal_slope = first(temporal_slope),  # already constant per MSA
    .groups = "drop"
  )


# Merge df_quad with df_MSA to get HPA and rent to income data
df_quad <- df_quad %>%
  left_join(
    df_MSA %>%
      dplyr::select(
        GEOID,
        `HPI with 2000 base`,
        RentToIncome
      ),
    by = c("MAS_Code" = "GEOID")
  )


### ----------------------------------------------------------------------------
### Plot of C1, C2, C3, C4 for Type 1 MSAs -------------------------------------

df_quad <- df_quad %>%
  mutate(
    quadrant = case_when(
      pop_change_pct >= 0 & temporal_slope >= 0 ~ "C1",
      pop_change_pct < 0 & temporal_slope >= 0 ~ "C2",
      pop_change_pct < 0 & temporal_slope < 0 ~ "C3",
      pop_change_pct >= 0 & temporal_slope < 0 ~ "C4"
    ))

quad_colors <- c("C1" = "#0072B2", "C2" = "#D55E00", "C3" = "#009E73", "C4" = "#CC79A7")



### Plot of beta for Type 1 MSAs and their population change (%)
p <- ggplot(df_quad, aes(x = pop_change_pct, y = temporal_slope, color = quadrant)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(size = 1.3, alpha = 0.9) +
  scale_color_manual(values = quad_colors) +
  scale_x_continuous(limits = c(-20, 40), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-20, 20), expand = c(0, 0)) +
  labs(x = "Population change (%)", y = "Longitudinal exponent") +
  theme_bw(base_size = 10) +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"), #t,r,b,l
    panel.grid = element_blank(),
    legend.position = "none",
    text = element_text(family = "Arial"),
  )

### Boxplot of HPA for Type 1 MSAs
p_hpa <- ggplot(df_quad, aes(x = quadrant, y = `HPI with 2000 base`, fill = quadrant, color = quadrant)) +
  geom_boxplot(width = 0.6, alpha = 0.3, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 0.5, alpha = 0.6) +
  scale_fill_manual(values = quad_colors) +
  scale_color_manual(values = quad_colors) +
  scale_y_continuous(limits = c(100, 400), expand = c(0, 0)) +
  labs(x = NULL, y = "HPA") +
  theme_classic(base_size = 9) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    panel.grid = element_blank(),
    legend.position = "none",
    text = element_text(family = "Arial")
  )


### Boxplot of median rent-to-income for Type 1 MSAs
p_rent <- ggplot(df_quad, aes(x = quadrant, y = RentToIncome, fill = quadrant, color = quadrant)) +
  geom_boxplot(width = 0.6, alpha = 0.3, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 0.5, alpha = 0.6) +
  scale_fill_manual(values = quad_colors) +
  scale_color_manual(values = quad_colors) +
  scale_y_continuous(limits = c(20, 40), expand = c(0, 0)) +
  labs(x = NULL, y = "Rent-to-income (%)") +
  theme_classic(base_size = 9) +
  theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    panel.grid = element_blank(),
    legend.position = "none",
    text = element_text(family = "Arial")
  )

panel_b <- p_hpa / p_rent

p_final <- (p | panel_b) +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 12, face = "bold"),
    text = element_text(family = "Arial")
  )


p_final
























