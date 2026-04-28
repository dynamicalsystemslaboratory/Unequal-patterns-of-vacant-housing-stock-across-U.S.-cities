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

setwd("")   # Path for working folder


#---------------------------------------------------------------------------------------------------------
#------------------------------------------------------------ DATA ---------------------------------------
data <- readRDS("Data/clean_data.rds")
ACSD_data <- readRDS("Data/ACSD house/ACSD_data_clean.rds")

MSA <- read_excel("Data/qcew-county-msa-csa-crosswalk.xlsx", sheet = 3)

#---------------------------------------------------------------------------------------------------------
#------------------------------- MISC --------------------------------------------------------------------
extract_scaling_results <- function(df_source, area = c("County", "MSA"), outcome = c("Vacant", "Occupied", "Total")) {
  area <- match.arg(area)
  outcome <- match.arg(outcome)
  
  all_results <- lapply(2010:2022, function(yr) {
    df_year_pre <- df_source %>% filter(year == yr)
    
    if (area == "MSA") {
      df_year_pre <- df_year_pre %>%
        group_by(MAS_Code) %>%
        summarize(
          Vacant = sum(Vacant, na.rm = TRUE),
          Occupied = sum(Occupied, na.rm = TRUE),
          Total = sum(Total, na.rm = TRUE),
          POPESTIMATE = sum(POPESTIMATE, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    total_units <- nrow(df_year_pre)
    
    df_year <- df_year_pre %>%
      filter(.data[[outcome]] != 0, .data[["POPESTIMATE"]] != 0, .data[["Total"]] != 0)
    
    null_units <- total_units - nrow(df_year)
    
    f_total <- as.formula(paste0("log(", outcome, ") ~ log(Total)"))
    f_pop   <- as.formula(paste0("log(", outcome, ") ~ log(POPESTIMATE)"))
    
    m1  <- lm(f_total, data = df_year)
    m2  <- lm(f_pop,   data = df_year)

    ct1 <- lmtest::coeftest(m1, vcov = vcovHC(m1, type="HC2"))
    ct2 <- lmtest::coeftest(m2, vcov = vcovHC(m2, type="HC2"))
    
    # robust SEs
    se_alpha2 <- ct2[1, 2]
    se_beta2  <- ct2[2, 2]
    
    ci1 <- confint(ct1, level = 0.95)
    ci2 <- confint(ct2, level = 0.95)
    
    beta1  <- coef(m1)[2]
    alpha1 <- coef(m1)[1]
    beta2  <- coef(m2)[2]
    alpha2 <- coef(m2)[1]
    
    alpha_CI2 <- ci2[1, ]
    beta_CI2  <- ci2[2, ]
    
    r2_1 <- summary(m1)$adj.r.squared
    r2_2 <- summary(m2)$adj.r.squared
    
    data.frame(
      year = yr,
      model = c("pop"),
      beta = sprintf("%.3f", c(beta2)),
      beta_CI = c(sprintf("[%.3f;%.3f]", beta_CI2[1], beta_CI2[2])),
      alpha = sprintf("%.3f", c(alpha2)),
      alpha_CI = c(sprintf("[%.3f;%.3f]", alpha_CI2[1], alpha_CI2[2])),
      adj_r2 = sprintf("%.3f", c(r2_2)),
      total_units = total_units,
      null_units = null_units,
      beta_se = sprintf("%.10f", se_beta2),  # add robust SE column
      alpha_se = sprintf("%.10f", se_alpha2),
      stringsAsFactors = FALSE
    )
  })
  
  final_results <- bind_rows(all_results) %>%
    mutate(
      beta_lower = as.numeric(sub("\\[([^;]+);.*", "\\1", beta_CI)),
      beta_upper = as.numeric(sub(".*;([^]]+)\\]", "\\1", beta_CI)),
      alpha_lower = as.numeric(sub("\\[([^;]+);.*", "\\1", alpha_CI)),
      alpha_upper = as.numeric(sub(".*;([^]]+)\\]", "\\1", alpha_CI))
    )
  
  return(final_results)
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


## Test alternative definitions of vacancy
# df$Vacant <- df$Vacant - df$For_seasonal_recreational_or_occasional_use #- df$Sold_not_occupied - df$Rented_not_occupied
# dfMSA$Vacant <- dfMSA$Vacant - dfMSA$For_seasonal_recreational_or_occasional_use #- dfMSA$Sold_not_occupied - dfMSA$Rented_not_occupied

#df$Vacant <- df$Other_vacant
#dfMSA$Vacant <- dfMSA$Other_vacant

## Using USPS data
# df$Vacant <- df$res_vac
# dfMSA$Vacant <- dfMSA$res_vac



# ------------------------------------------------------------------------------
### Analysis -------------------------------------------------------------------
t_county_occ <- extract_scaling_results(df, area = "County", outcome = "Occupied")
t_msa_occ  <- extract_scaling_results(dfMSA, area = "MSA", outcome = "Occupied")
t_county_vac <- extract_scaling_results(df, area = "County", outcome = "Vacant")
t_msa_vac  <- extract_scaling_results(dfMSA, area = "MSA", outcome = "Vacant")
t_county_tot <- extract_scaling_results(df, area = "County", outcome = "Total")
t_msa_tot  <- extract_scaling_results(dfMSA, area = "MSA", outcome = "Total")


# Combine all results into one tidy dataframe 
all_results <- bind_rows(
  t_msa_occ %>% mutate(area = "MSA", outcome = "Occupied"),
  t_msa_vac %>% mutate(area = "MSA", outcome = "Vacant"),
  t_msa_tot %>% mutate(area = "MSA", outcome = "Total")
)

all_results <- all_results %>%
  mutate(
    year = as.numeric(year),
    beta = as.numeric(beta),
    beta_lower = as.numeric(beta_lower),
    beta_upper = as.numeric(beta_upper),
    beta_se = as.numeric(beta_se),
    alpha = as.numeric(alpha),
    alpha_lower = as.numeric(alpha_lower),
    alpha_upper = as.numeric(alpha_upper),
    alpha_se = as.numeric(alpha_se)
  )

# Plot all exponents together --------------------------------------------------
p_slope <- ggplot(all_results, aes(x = year, y = beta, color = outcome, linetype = area, shape = outcome)) +
  geom_line(size = 0.2) +
  geom_point(size = 1) + #1
  geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), size = 0.2, width = 0.2) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "black", size = 0.5) +
  scale_color_manual(values = c("Occupied" = "#3f4e2a",
                                "Vacant" = "#602f42",
                                "Total" = "#8c510a")) +
  #"Total" = "#13265C")) +
  scale_shape_manual(values = c("Occupied" = 17, "Vacant" = 15, "Total" = 19)) +
  labs(x = "Year", y = expression(beta(t)), color = "Outcome", shape = "Outcome") +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),   #t,r,b,l
    #legend.position = "top",
    legend.position = c(0.05, 0.95),
    legend.justification = c("left", "top"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    text = element_text(family = "Arial")
  ) +
  scale_x_continuous(breaks = unique(all_results$year), labels = unique(all_results$year)) +
  scale_y_continuous(limits = c(0.8, 1.1), breaks = seq(0.8, 1.1, by = 0.05), expand = c(0, 0))
p_slope


# Plot intercept
p_intercept <- ggplot(all_results, aes(x = year, y = alpha, color = outcome, linetype = area, shape = outcome)) +
  geom_line(size = 0.2) +
  geom_point(size = 1) + #1
  geom_errorbar(aes(ymin = alpha_lower, ymax = alpha_upper), size = 0.2, width = 0.2) +
  #geom_hline(yintercept = 1.0, linetype = "dashed", color = "black", size = 0.5) +
  scale_color_manual(values = c("Occupied" = "#3f4e2a",
                                "Vacant" = "#602f42",
                                "Total" = "#8c510a")) +
  scale_shape_manual(values = c("Occupied" = 17, "Vacant" = 15, "Total" = 19)) +
  labs(x = "Year", y = expression(Y[0](t)), color = "Outcome", shape = "Outcome") +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),   #t,r,b,l
    legend.position = c(0.05, 0.95),
    legend.justification = c("left", "top"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    text = element_text(family = "Arial")
  ) +
  scale_x_continuous(breaks = unique(all_results$year), labels = unique(all_results$year)) +
  scale_y_continuous(limits = c(-3, 0), breaks = seq(-3, 0, by = 0.5), expand = c(0, 0))
p_intercept







### ----------------------------------------------------------------------------
# Test if there is a time effect

# # SI: Total time effects -------------------------------------------------------
# model_Time_MSA <- lm(log(Total) ~ log(POPESTIMATE) + log(POPESTIMATE)*factor(year), data = dfMSA)  
# model_diffIntercepts <- lm(log(Total) ~ log(POPESTIMATE) + factor(year), data = dfMSA)  
# 
# anova(model_diffIntercepts, model_Time_MSA)
# 
# # SI: Total time effects -------------------------------------------------------
# model_Time_MSA <- lm(log(Occupied) ~ log(POPESTIMATE) + log(POPESTIMATE)*factor(year), data = dfMSA)   
# model_diffIntercepts <- lm(log(Occupied) ~ log(POPESTIMATE) + factor(year), data = dfMSA)  
# 
# anova(model_diffIntercepts, model_Time_MSA)


# Vacant time effects ----------------------------------------------------------
model_noTime_MSA1 <- lm(log(Vacant) ~ log(POPESTIMATE), data = dfMSA)                 # 1 intercept & 1 slope
model_Time_MSA1 <- lm(log(Vacant) ~ log(POPESTIMATE):factor(year), data = dfMSA)      # 1 intercept & dif slopes 
model_Time_MSA <- lm(log(Vacant) ~ log(POPESTIMATE) + log(POPESTIMATE)*factor(year), data = dfMSA)   # dif intercept & dif slopes
model_diffIntercepts <- lm(log(Vacant) ~ log(POPESTIMATE) + factor(year), data = dfMSA)  # dif intercept & 1 slope


anova(model_diffIntercepts, model_Time_MSA)

vcov_robust <- vcovHC(model_Time_MSA, type = "HC2")
trend_years <- emmeans::emtrends(model_Time_MSA, 
                        specs = ~ year, 
                        var = "log(POPESTIMATE)")




# # Convert emtrends object to a data frame
# df_trend <- as.data.frame(trend_years)
# 
# # The typical columns will be: year, log(POPESTIMATE).trend (the slope), SE, df, lower.CL, upper.CL
# 
# ggplot(df_trend, aes(x = year, y = `log(POPESTIMATE).trend`)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
#   geom_smooth(method = "lm", se = FALSE, color = "blue") +
#   theme_minimal() +
#   labs(
#     x = "Year",
#     y = "Slope"
#   )



## ------------------------------------------------------------------------------
## Slope and intercept extrapolations ------------------------------------------

t_msa_vac$beta <- as.numeric(t_msa_vac$beta)
t_msa_vac$alpha <- as.numeric(t_msa_vac$alpha)

beta_SE = as.numeric(t_msa_vac$beta_se)
inv_varBeta = 1 / beta_SE^2

alpha_SE = as.numeric(t_msa_vac$alpha_se)
inv_varAlpha = 1 / alpha_SE^2

## Slope beta -------------------------------------------------------------------
#--- Linear 
Beta_weighted_trend <- lm(beta ~ year, data = t_msa_vac, weights = inv_varBeta) #inverse variance weighting
summary(Beta_weighted_trend)

Beta_pred_2050 <- predict(Beta_weighted_trend,
                          newdata = data.frame(year = 2050), interval = "confidence",
                          level = 0.95)
Beta_pred_2050


#--- Spline
Beta_weighted_spline <- lm(beta ~ ns(year, df = 4), data = t_msa_vac, weights = inv_varBeta)
summary(Beta_weighted_spline)

Beta_pred_2050_spline <- predict(Beta_weighted_spline,
                                 newdata = data.frame(year = 2050),
                                 interval = "confidence",
                                 level = 0.95)
Beta_pred_2050_spline








