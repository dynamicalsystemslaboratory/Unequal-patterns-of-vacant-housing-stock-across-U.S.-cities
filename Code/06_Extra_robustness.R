library(dplyr)
library(sf)
library(patchwork)
library(extrafont)
library(ggplot2)
library(readxl)
library(lmtest)
library(sandwich)
library(stringr)
library(purrr)
library(ineq)
library(emmeans)
library(scales)

loadfonts(device = "win") 
fonts()

setwd("") # Path for working folder


#---------------------------------------------------------------------------------------------------------
#------------------------------------------------------------ DATA ---------------------------------------

## Housing Price Index (HPI) from FHFA (https://www.fhfa.gov/data/hpi/datasets?tab=annual-data)
# The HPI can be used to compute HPA as: HPA_t = (HPI_t - HPI_t-1)/(HPI_t_1). In this dataset this is equivalent to Annual Change (%) column
hpi_data <- read_excel("Data/hpi_at_cbsa.xlsx", skip = 5)

## Rent-to-income ratio from ACS (https://data.census.gov/table?q=B25071&g=010XX00US$31000M1)
# 2022 5-year estimates. B25071 (median gross rent as % of income)
rent_income_data <- read.csv("Data/ACSDT5Y2022.B25071-Data.csv") 

data <- readRDS("Data/clean_data.rds")
ACSD_data <- readRDS("Data/ACSD house/ACSD_data_clean.rds")
Census2020 <- read_excel("Data/DECENNIALPL2020.H1-2025-03-26T211423.xlsx", sheet = 2)

# Crosswalks 
MSA <- read_excel("Data/qcew-county-msa-csa-crosswalk.xlsx", sheet = 3)

# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2022.html#list-tab-790442341
cbsa_shp22 <- st_read("Data/tl_2023_us_cbsa/tl_2023_us_cbsa.shp")
cbsa_shp20 <- st_read("Data/tl_2020_us_cbsa/tl_2020_us_cbsa.shp") 
county_shp20 <- st_read("Data/tl_2020_us_county/tl_2020_us_county.shp")


# Data processing ----------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

# Remove Alaska, Puerto Rico and Hawaii
data <- subset(data, !grepl("Puerto Rico|Hawaii|Alaska", GeographicArea))

# Add population growth rate
data <- data %>%
  group_by(County_code) %>%
  mutate(
    pop_2010 = POPESTIMATE[year == 2010][1],
    pop_2022 = POPESTIMATE[year == 2022][1],
    pop_growth_rate = ((pop_2022) - (pop_2010)) / (pop_2010)
  ) %>%
  ungroup()


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
    pop_growth_rate = sum(pop_growth_rate, na.rm = TRUE),
    .groups = "drop"
  )


# Remove last zero from shapefile (ALAND is in m^2)
cbsa_shp22 <- cbsa_shp22 %>%
  mutate(GEOID = paste0("C", substr(GEOID, 1, nchar(GEOID) - 1)))


# Merge with shapefile
df_MSA <- cbsa_shp22 %>%
  left_join(dfMSA_aux, by = c("GEOID" = "MAS_Code"))

df_MSA <- df_MSA %>% filter(!is.na(POPESTIMATE))


# Clean the HPI data 
hpi_data <- hpi_data %>%
  filter(!grepl("non CBSA areas", Name, ignore.case = TRUE))

hpi_data <- hpi_data[hpi_data$Year == 2022, ] 
hpi_data <- hpi_data %>% dplyr::select(-Year, -Name)

# Merge with df_MSA
df_MSA <- df_MSA %>%
  left_join(hpi_data, by = c("CBSAFP" = "CBSA"))


# Rent-to-income data 
rent_income_data <- rent_income_data[, -ncol(rent_income_data)]
rent_income_data <- rent_income_data[-1, ]

rent_income_data <- rent_income_data %>%
  rename(CBSA = GEO_ID,
         RentToIncome = B25071_001E,
         MarginOfError = B25071_001M)

rent_income_data <- rent_income_data %>%
  mutate(
    RentToIncome = as.numeric(RentToIncome),
    MarginOfError = as.numeric(MarginOfError)
  )

rent_income_data <- rent_income_data %>%
  mutate(CBSA = str_extract(CBSA, "\\d{5}$"))

rent_income_data <- rent_income_data %>% dplyr::select(-NAME)

df_MSA <- df_MSA %>%
  left_join(rent_income_data, by = c("CBSAFP" = "CBSA"))






# ------------------------------------------------------------------------------
### Analysis -------------------------------------------------------------------




### ----------------------------------------------------------------------------
### Bootstrap analysis to verify robustness of transversal scaling laws --------


### Function to estimate beta from subsample -----------------------------------
estimate_beta <- function(data, Y, prop) {
  n <- nrow(data)
  idx <- sample(seq_len(n), size = floor(prop * n), replace = FALSE)
  d <- data[idx, ]
  f <- as.formula(paste0("log(", Y, ") ~ log(POPESTIMATE)"))
  fit <- lm(f, data = d)
  coef(fit)[2]   
}


# Run simulations
set.seed(73)

Nruns <- 10000
proportion <- 0.3  # 0-1

beta_occup_samples <- replicate(Nruns, estimate_beta(data=df_MSA, Y="Occupied", prop=proportion))
beta_occup_mean <- mean(beta_occup_samples, na.rm = TRUE)
beta_occup_sd   <- sd(beta_occup_samples, na.rm = TRUE)

beta_vac_samples <- replicate(Nruns, estimate_beta(data = df_MSA, Y = "Vacant", prop = proportion))
beta_vac_mean <- mean(beta_vac_samples, na.rm = TRUE)
beta_vac_sd   <- sd(beta_vac_samples, na.rm = TRUE)

beta_df <- data.frame(beta_occup = beta_occup_samples)
beta_df$beta_vac <- beta_vac_samples



### Plots
### Plot distribution of betas for vacant --------------------------------------
p_vac_Boot <- ggplot(beta_df, aes(x = beta_vac)) +
  geom_histogram(bins = 50, fill = "#9b536d", color = "white", alpha = 0.9) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = beta_vac_mean) +
  annotate("text", x = -Inf, y = -Inf,
           label = sprintf("Mean = %.3f\nSD = %.3f", beta_vac_mean, beta_vac_sd),
           hjust = -0.2, vjust = -8, size = 4) +
  labs(x = expression(beta), y = "Frequency") +
  scale_x_continuous(limits = c(0.75, 1.05), breaks = seq(0.7, 1.05, by = 0.05), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 200), expand = c(0, 0)) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.4, 0.4, 0.2, 0.2, "cm"), #t,r,b,l
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

### Plot distribution of betas for vacant --------------------------------------
p_occup_Boot <- ggplot(beta_df, aes(x = beta_occup)) +
  geom_histogram(bins = 50, fill = "#7f9e5d", color = "white", alpha = 0.9) +
  #geom_density(linewidth = 0.9) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = beta_occup_mean) +
  annotate("text", x = -Inf, y = -Inf, 
           label = sprintf("Mean = %.3f\nSD = %.3f", beta_occup_mean, beta_occup_sd),
           hjust = -0.2, vjust = -8, size = 4) +
  labs(x = expression(beta), y = "Frequency") +
  scale_x_continuous(limits = c(0.96, 1.02), breaks = seq(0.96, 1.02, by = 0.01), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 200), expand = c(0, 0)) +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.4, 0.4, 0.2, 0.2, "cm"), #t,r,b,l
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

p_Boot <- (p_occup_Boot + p_vac_Boot) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 12, face = "bold"),
    theme(text = element_text(family = "Arial"))
  )
p_Boot

#ggsave(filename = "SI_Fig_1.pdf", plot = p_Boot, width = 170, height = 70, units = "mm", dpi = 900, device = cairo_pdf)





### ----------------------------------------------------------------------------
### Testing the existence of a global scaling relationship ---------------------


# Split MSAs into 2 groups according to:

# 1) Population growth
df_MSA <- df_MSA %>%
  mutate(
    growth_group = ifelse(pop_growth_rate > 0, "Growing", "Declining"),
    growth_group = factor(growth_group))

# 2) Rent-to-income
df_MSA <- df_MSA %>%
  mutate(
    rent_group = ifelse(RentToIncome > median(RentToIncome, na.rm = TRUE), "High stress","Low stress"),
    rent_group = factor(rent_group))

# 3) HPA
df_MSA <- df_MSA %>%
  mutate(
    hpi_group = ifelse(`HPI with 2000 base` > median(`HPI with 2000 base`, na.rm = TRUE), "High HPI","Low HPI"),
    hpi_group = factor(hpi_group))




df_MSA$logN <- log(df_MSA$POPESTIMATE)
df_MSA$logY <- log(df_MSA$Vacant)


# Fit simple and full models according to HPA ----------------------------------
model_interaction <- lm(logY ~ logN * hpi_group, data = df_MSA)
summary(model_interaction)

emmeans(model_interaction, ~ hpi_group, at = list(logN = 0), vcov. = vcovHC(model_interaction, type = "HC2")) #group-specific intercept
emtrends(model_interaction, ~ hpi_group, var = "logN",vcov. = vcovHC(model_interaction, type = "HC2"))        #group-specific slope


# Compare with model without interaction
model_no_interaction <- lm(logY ~ logN + hpi_group, data = df_MSA)
summary(model_no_interaction)

emmeans(model_no_interaction, ~ hpi_group, at = list(logN = 0), vcov. = vcovHC(model_no_interaction, type = "HC2")) #group-specific intercept


anova(model_no_interaction, model_interaction)







# Fit simple and full models according to rent_group ---------------------------
model_interaction <- lm(logY ~ logN * rent_group, data = df_MSA)
summary(model_interaction)

emmeans(model_interaction, ~ rent_group, at = list(logN = 0), vcov. = vcovHC(model_interaction, type = "HC2")) #group-specific intercept
emtrends(model_interaction, ~ rent_group, var = "logN",vcov. = vcovHC(model_interaction, type = "HC2"))        #group-specific slope


# Compare with model without interaction
model_no_interaction <- lm(logY ~ logN + rent_group, data = df_MSA)
summary(model_no_interaction)

emmeans(model_no_interaction, ~ rent_group, at = list(logN = 0), vcov. = vcovHC(model_no_interaction, type = "HC2")) #group-specific intercept


anova(model_no_interaction, model_interaction)


# Fit simple and full models according to growth_group ---------------------------
model_interaction <- lm(logY ~ logN * growth_group, data = df_MSA)
summary(model_interaction)

emmeans(model_interaction, ~ growth_group, at = list(logN = 0), vcov. = vcovHC(model_interaction, type = "HC2")) #group-specific intercept
emtrends(model_interaction, ~ growth_group, var = "logN",vcov. = vcovHC(model_interaction, type = "HC2"))        #group-specific slope


# Compare with model without interaction
model_no_interaction <- lm(logY ~ logN + growth_group, data = df_MSA)
summary(model_no_interaction)

emmeans(model_no_interaction, ~ growth_group, at = list(logN = 0), vcov. = vcovHC(model_no_interaction, type = "HC2")) #group-specific intercept


anova(model_no_interaction, model_interaction)





# ### Get all slopes for the interaction and no interaction model ----------------
# df_MSA$log_POPESTIMATE <- log(df_MSA$POPESTIMATE)
# df_MSA$log_Vacant <- log(df_MSA$Vacant)
# 
# get_slope_table <- function(data, group_var) {
#   # formulas
#   form_int <- as.formula(paste0("log_Vacant ~ log_POPESTIMATE * ", group_var))
#   form_no_int <- as.formula(paste0("log_Vacant ~ log_POPESTIMATE + ", group_var))
#   
#   model_int <- lm(form_int, data = data)
#   model_no_int <- lm(form_no_int, data = data)
#   
#   # group-specific slopes from interaction model
#   tab_int <- emtrends(
#     model_int,
#     specs = as.formula(paste0("~ ", group_var)),
#     var = "log_POPESTIMATE",
#     vcov. = vcovHC(model_int, type = "HC2")
#   ) %>%
#     as.data.frame()
#   
#   names(tab_int)[1] <- "group"
#   
#   tab_int <- tab_int %>%
#     rename(
#       slope = log_POPESTIMATE.trend,
#       SE_HC2 = SE
#     ) %>%
#     mutate(
#       model = "Interaction",
#       grouping_variable = group_var,
#       .before = 1
#     ) %>%
#     select(grouping_variable, model, group, slope, SE_HC2, df, lower.CL, upper.CL)
#   
#   # common slope from no-interaction model
#   tab_no_int <- emtrends(
#     model_no_int,
#     specs = ~ 1,
#     var = "log_POPESTIMATE",
#     vcov. = vcovHC(model_no_int, type = "HC2")
#   ) %>%
#     as.data.frame()
#   
#   tab_no_int <- tab_no_int %>%
#     rename(
#       slope = log_POPESTIMATE.trend,
#       SE_HC2 = SE
#     ) %>%
#     mutate(
#       grouping_variable = group_var,
#       model = "No interaction",
#       group = "Common slope",
#       .before = 1
#     ) %>%
#     select(grouping_variable, model, group, slope, SE_HC2, df, lower.CL, upper.CL)
#   bind_rows(tab_no_int, tab_int)
# }
# 
# tab_growth <- get_slope_table(df_MSA, "growth_group")
# tab_rent <- get_slope_table(df_MSA, "rent_group")
# tab_hpi <- get_slope_table(df_MSA, "hpi_group")
# 
# tab_growth
# tab_rent
# tab_hpi




# Create grouping variables ----------------------------
df_MSA <- df_MSA %>%
  mutate(
    growth_group = ifelse(pop_growth_rate > 0, "Growing", "Declining"),
    growth_group = factor(growth_group, levels = c("Declining", "Growing")),
    
    rent_group = ifelse(RentToIncome > median(RentToIncome, na.rm = TRUE), "High", "Low"),
    rent_group = factor(rent_group, levels = c("Low", "High")),
    
    hpi_group = ifelse(`HPI with 2000 base` > median(`HPI with 2000 base`, na.rm = TRUE), "High", "Low"),
    hpi_group = factor(hpi_group, levels = c("Low", "High"))
  )


### Plot transversal scaling for MSAs splitted according to their groups
plot_interaction_group <- function(data, group_var, group_name, colors) {

  form_interaction <- as.formula(paste0("log10(Vacant) ~ log10(POPESTIMATE) * ", group_var))
  form_no_interaction <- as.formula(paste0("log10(Vacant) ~ log10(POPESTIMATE) + ", group_var))
  
  # fit models
  model_interaction <- lm(form_interaction, data = data)
  model_no_interaction <- lm(form_no_interaction, data = data)
  
  # prediction grid
  group_levels <- levels(data[[group_var]])
  
  df_pred_interaction <- expand.grid(
    POPESTIMATE = exp(seq(log(10^4), log(10^8), length.out = 200)),
    group_temp = group_levels
  )
  names(df_pred_interaction)[2] <- group_var
  
  df_pred_no_interaction <- expand.grid(
    POPESTIMATE = exp(seq(log(10^4), log(10^8), length.out = 200)),
    group_temp = group_levels
  )
  names(df_pred_no_interaction)[2] <- group_var
  
  # predictions
  df_pred_interaction$pred_interaction <- predict(model_interaction, newdata = df_pred_interaction)
  df_pred_no_interaction$pred_no_interaction <- predict(model_no_interaction, newdata = df_pred_no_interaction)
  
  # plot
  p <- ggplot(data, aes(x = POPESTIMATE, y = Vacant, color = .data[[group_var]])) +
    geom_point(alpha = 0.3, size = 0.6) +
    geom_line(
      data = df_pred_no_interaction %>% filter(.data[[group_var]] == group_levels[1]),
      aes(x = POPESTIMATE, y = 10^pred_no_interaction), inherit.aes = FALSE,linetype = "dashed",color = "black", linewidth = 0.6) +
    geom_line(data = df_pred_interaction, aes(y = 10^pred_interaction, group = .data[[group_var]]),linewidth = 0.6) +
    scale_x_log10(labels = trans_format("log10", math_format(10^.x)), limits = c(10^4, 10^8), expand = c(0, 0)) +
    scale_y_log10( labels = trans_format("log10", math_format(10^.x)), limits = c(10^3, 10^7), expand = c(0, 0)) +
    scale_color_manual(name = group_name, values = colors) +
    labs(x = "Population", y = "Vacant housing units") +
    theme_classic(base_size = 9) +
    theme(
      legend.position = c(0.03, 0.97),
      legend.justification = c(0, 1),
      legend.direction = "vertical",
      legend.title = element_text(size = 5),
      legend.text = element_text(size = 5),
      plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
      text = element_text(family = "Arial")
    )
  return(p)
}

p_growth <- plot_interaction_group(
  data = df_MSA,
  group_var = "growth_group",
  group_name = "Population growth",
  colors = c("Declining" = "#D55E00", "Growing" = "#009E73")
)

p_rent <- plot_interaction_group(
  data = df_MSA,
  group_var = "rent_group",
  group_name = "Rent burden",
  colors = c("Low" = "#4E79A7", "High" = "#E15759")
)

p_hpi <- plot_interaction_group(
  data = df_MSA,
  group_var = "hpi_group",
  group_name = "HPA",
  colors = c("Low" = "#7B61FF", "High" = "#2A9D8F")
)


p_split <- ( p_hpi | p_rent | p_growth) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 11, face = "bold"),
    text = element_text(family = "Arial")
  )

p_split

#ggsave(filename = "SI_Fig_4.pdf", plot = p_split, width = 150, height = 60, units = "mm", dpi = 900, device = cairo_pdf)





### ----------------------------------------------------------------------------
### Scaling with population density --------------------------------------------


Census2020 <- Census2020[!is.na(Census2020$Total), ]

Census2020$Total_Census <- as.numeric(gsub(",", "", Census2020$Total))
Census2020$Occupied_Census <- as.numeric(gsub(",", "", Census2020$Occupied))
Census2020$Vacant_Census <- as.numeric(gsub(",", "", Census2020$Vacant))

Census2020 <- Census2020 %>%
  dplyr::select(-Total, -Occupied, -Vacant)

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


### ----------------------------------------------------------------------------
### Scaling with population density --------------------------------------------

cbsa_shp20 <- cbsa_shp20 %>%
  mutate(GEOID = paste0("C", substr(GEOID, 1, nchar(GEOID) - 1)))

# Merge with shapefile
dfMSACensus <- cbsa_shp20 %>%
  left_join(dfMSACensus, by = c("GEOID" = "MAS_Code"))

dfMSACensus <- dfMSACensus %>% filter(!is.na(POPESTIMATE))


# Scaling analysis
dfMSACensus$Area <- (dfMSACensus$ALAND) / 1000000 #convert to km2
dfMSACensus$pop_density <- dfMSACensus$POPESTIMATE / dfMSACensus$Area


Census2020 <- lm(log(Vacant_Census) ~ log(pop_density), data = dfMSACensus)
summary(Census2020)
ci <- confint(lmtest::coeftest(Census2020, vcov = vcovHC(Census2020, type="HC2")), level = 0.95)
ci





# Scaling for all counties - 2020 Census ---------------------------------------

# Merge with shapefile
census_df <- county_shp20 %>%
  left_join(census_df, by = c("GEOID" = "County_code"))

census_df <- census_df %>% filter(!is.na(POPESTIMATE))


# Scaling analysis
census_df$Area <- (census_df$ALAND) / 1000000 #convert to km2
census_df$pop_density <- census_df$POPESTIMATE / census_df$Area


Census2020_county <- lm(log(Vacant_Census) ~ log(pop_density), data = census_df)
summary(Census2020_county)
ci <- confint(lmtest::coeftest(Census2020_county, vcov = vcovHC(Census2020_county, type="HC2")), level = 0.95)
ci














