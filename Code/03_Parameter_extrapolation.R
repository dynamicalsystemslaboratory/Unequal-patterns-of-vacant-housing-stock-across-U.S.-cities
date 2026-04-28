
#----------------------------------------------------------
# RUN 02_Transversal_scaling_overTime.R before running this script

setwd("")  
source("Code/02_Transversal_scaling_overTime.R")
#----------------------------------------------------------

library(ggrepel)


# Slope and intercept extrapolations ---------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------

t_msa_vac$beta <- as.numeric(t_msa_vac$beta)
t_msa_vac$alpha <- as.numeric(t_msa_vac$alpha)

beta_SE = as.numeric(t_msa_vac$beta_se)
inv_varBeta = 1 / beta_SE^2

alpha_SE = as.numeric(t_msa_vac$alpha_se)
inv_varAlpha = 1 / alpha_SE^2



## Beta (slope) ----------------------------------------------------------------


Beta_weighted_trend <- lm(beta ~ year, data = t_msa_vac, weights = inv_varBeta) # Linear
Beta_weighted_spline <- lm(beta ~ ns(year, df = 4), data = t_msa_vac, weights = inv_varBeta) # Spline

# Create grid to 2050
years_extended <- tibble(year = seq(min(t_msa_vac$year, na.rm = TRUE), 2050, by = 1))


### Get fitted vector for each year and 2050 CI matrices 
# Predict full series (no interval) for plotting
Beta_pred_linear_vec <- predict(Beta_weighted_trend, newdata = years_extended)
Beta_pred_spline_vec <- predict(Beta_weighted_spline, newdata = years_extended)

# 95% confidence interval for 2050
pred_2050_new <- data.frame(year = 2050)
Beta_pred_2050_linear <- predict(Beta_weighted_trend, newdata = pred_2050_new, interval = "confidence", level = 0.95)
Beta_pred_2050_spline <- predict(Beta_weighted_spline, newdata = pred_2050_new, interval = "confidence", level = 0.95)

Beta_pred_2050_linear <- as.numeric(Beta_pred_2050_linear[1, ])
Beta_pred_2050_spline <- as.numeric(Beta_pred_2050_spline[1, ])

Beta_pred_2050_linear_val <- Beta_pred_2050_linear[1] 
Beta_pred_2050_spline_val <- Beta_pred_2050_spline[1]

## Predictions 
pred_lin_ci <- predict(Beta_weighted_trend, newdata = years_extended, interval = "confidence", level = 0.95)

# Linear
pred_lin_df <- tibble(
  year = years_extended$year,
  model = "Linear",
  fit  = as.numeric(pred_lin_ci[, "fit"]),
  lwr  = as.numeric(pred_lin_ci[, "lwr"]),
  upr  = as.numeric(pred_lin_ci[, "upr"])
)

# Spline 
pred_spline_ci <- predict(Beta_weighted_spline, newdata = years_extended, interval = "confidence", level = 0.95)
pred_spline_df <- tibble(
  year = years_extended$year,
  model = "Spline",
  fit  = as.numeric(pred_spline_ci[, "fit"]),
  lwr  = as.numeric(pred_spline_ci[, "lwr"]),
  upr  = as.numeric(pred_spline_ci[, "upr"])
)

Beta_preds_ci_df <- bind_rows(pred_lin_df, pred_spline_df) 

# aesthetics
model_colors <- c("Linear" = "pink", "Spline" = "#8C4A5C")
model_linetypes <- c("Linear" = "dashed", "Spline" = "dashed")



# SI: Plot with predicted lines to 2050 ----------------------------------------
obs_df <- all_results %>% filter(year <= 2022)

p_slope_comb <- ggplot() +
  # ribbons (CI) first so they're behind lines
  geom_ribbon(data = Beta_preds_ci_df, aes(x = year, ymin = lwr, ymax = upr, fill = model),
              alpha = 0.1, inherit.aes = FALSE) +
  geom_line(data = Beta_preds_ci_df, aes(x = year, y = fit, color = model, linetype = model),
            size = 0.3, inherit.aes = FALSE) +
  geom_errorbar(data = obs_df, aes(x = year, ymin = beta_lower, ymax = beta_upper, color = outcome),
                width = 0.3, size = 0.4, alpha = 0.6) +
  geom_point(data = obs_df, aes(x = year, y = beta, color = outcome, shape = outcome), size = 1) + #old size=1.8
  geom_line(data = obs_df, aes(x = year, y = beta, color = outcome, linetype = area), size = 0.6, alpha = 0.9) +
  geom_point(data = Beta_preds_ci_df %>% filter(year == 2050),
             aes(x = year, y = fit, color = model), size = 1, shape = 0, inherit.aes = FALSE) +
  geom_text_repel(data = Beta_preds_ci_df %>% filter(year == 2050),
                  aes(x = year, y = fit, label = sprintf("%s: %.3f", model, fit), color = model),
                  nudge_x = 1, direction = "y", hjust = 0, segment.size = 0.2, inherit.aes = FALSE) +

  geom_hline(yintercept = 1.0, linetype = "dashed", color = "black", size = 0.4) +
  scale_color_manual(values = c(
    "Occupied" = "#3f4e2a", "Vacant" = "#602f42", "Total" = "#8c510a",
    model_colors
  )) +
  scale_fill_manual(values = model_colors, guide = FALSE) +
  scale_shape_manual(values = c("Occupied" = 17, "Vacant" = 15, "Total" = 19)) +
  scale_linetype_manual(values = c("MSA" = "solid", model_linetypes)) +
  labs(x = "Year", y = expression(beta(t)), color = "Series / Model", shape = "Outcome") +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),   #t,r,b,l
    #legend.position = c(0.08, 1),
    legend.position = "none",
    legend.justification = c("left", "top"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    text = element_text(family = "Arial")
  ) +
  scale_x_continuous(breaks = seq(min(obs_df$year, na.rm = TRUE), 2050, by = 4), limits = c(min(obs_df$year, na.rm = TRUE), 2050)) +
  scale_y_continuous(limits = c(0.6, 1.05), breaks = seq(0.6, 1.05, by = 0.05), expand = c(0, 0))



## Y_0 (Intercept) -------------------------------------------------------------
Alpha_weighted_trend <- lm(alpha ~ year, data = t_msa_vac, weights = inv_varAlpha)
Alpha_weighted_spline <- lm(alpha ~ ns(year, df = 4), data = t_msa_vac, weights = inv_varAlpha)

years_extended <- tibble(year = seq(min(t_msa_vac$year, na.rm = TRUE), 2050, by = 1))

Alpha_pred_linear_vec <- predict(Alpha_weighted_trend, newdata = years_extended)
Alpha_pred_spline_vec <- predict(Alpha_weighted_spline, newdata = years_extended)

pred_2050_new <- data.frame(year = 2050)
Alpha_pred_2050_linear <- predict(Alpha_weighted_trend, newdata = pred_2050_new, interval = "confidence", level = 0.95)
Alpha_pred_2050_spline <- predict(Alpha_weighted_spline, newdata = pred_2050_new, interval = "confidence", level = 0.95)

Alpha_pred_2050_linear <- as.numeric(Alpha_pred_2050_linear[1, ])
Alpha_pred_2050_spline <- as.numeric(Alpha_pred_2050_spline[1, ])

Alpha_pred_2050_linear_val <- Alpha_pred_2050_linear[1] 
Alpha_pred_2050_spline_val <- Alpha_pred_2050_spline[1]


## Predictions
pred_lin_ci <- predict(Alpha_weighted_trend, newdata = years_extended, interval = "confidence", level = 0.95)
pred_lin_df <- tibble(
  year = years_extended$year,
  model = "Linear",
  fit  = as.numeric(pred_lin_ci[, "fit"]),
  lwr  = as.numeric(pred_lin_ci[, "lwr"]),
  upr  = as.numeric(pred_lin_ci[, "upr"])
)

pred_spline_ci <- predict(Alpha_weighted_spline, newdata = years_extended, interval = "confidence", level = 0.95)
pred_spline_df <- tibble(
  year = years_extended$year,
  model = "Spline",
  fit  = as.numeric(pred_spline_ci[, "fit"]),
  lwr  = as.numeric(pred_spline_ci[, "lwr"]),
  upr  = as.numeric(pred_spline_ci[, "upr"])
)

Alpha_preds_ci_df <- bind_rows(pred_lin_df, pred_spline_df)

model_colors <- c("Linear" = "pink", "Spline" = "#8C4A5C")
model_linetypes <- c("Linear" = "dashed", "Spline" = "dashed")


obs_df <- all_results %>% filter(year <= 2022)

# PLot
p_intercept_comb <- ggplot() +
  # ribbons (CI) first so they're behind lines
  geom_ribbon(data = Alpha_preds_ci_df, aes(x = year, ymin = lwr, ymax = upr, fill = model),
              alpha = 0.1, inherit.aes = FALSE) +
  geom_line(data = Alpha_preds_ci_df, aes(x = year, y = fit, color = model, linetype = model),
            size = 0.3, inherit.aes = FALSE) +
  geom_errorbar(data = obs_df, aes(x = year, ymin = alpha_lower, ymax = alpha_upper, color = outcome),
                width = 0.3, size = 0.4, alpha = 0.6) +
  geom_point(data = obs_df, aes(x = year, y = alpha, color = outcome, shape = outcome), size = 1) + #old size=1.8
  geom_line(data = obs_df, aes(x = year, y = alpha, color = outcome, linetype = area), size = 0.6, alpha = 0.9) +
  geom_point(data = Alpha_preds_ci_df %>% filter(year == 2050),
             aes(x = year, y = fit, color = model), size = 1, shape = 0, inherit.aes = FALSE) +
  geom_text_repel(data = Alpha_preds_ci_df %>% filter(year == 2050),
                  aes(x = year, y = fit, label = sprintf("%s: %.3f", model, fit), color = model),
                  nudge_x = 1, direction = "y", hjust = 0, segment.size = 0.2, inherit.aes = FALSE) +
  scale_color_manual(values = c(
    "Occupied" = "#3f4e2a", "Vacant" = "#602f42", "Total" = "#8c510a",
    model_colors
  )) +
  scale_fill_manual(values = model_colors, guide = FALSE) +
  scale_shape_manual(values = c("Occupied" = 17, "Vacant" = 15, "Total" = 19)) +
  scale_linetype_manual(values = c("MSA" = "solid", model_linetypes)) +
  labs(x = "Year", y = expression(ln(Y[0](t))), color = "Series / Model", shape = "Outcome") +
  theme_classic(base_size = 10) +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),  
    legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    text = element_text(family = "Arial")
  ) +
  scale_x_continuous(breaks = seq(min(obs_df$year, na.rm = TRUE), 2050, by = 4), limits = c(min(obs_df$year, na.rm = TRUE), 2050)) +
  scale_y_continuous(limits = c(-4, 1.5), breaks = seq(-4, 1.5, by = 0.5), expand = c(0, 0))
p_intercept_comb

p_combined <- (p_slope_comb  | p_intercept_comb) +
  theme(
    plot.tag = element_text(size = 12, face = "bold"),
    theme(text = element_text(family = "Arial"))
  )
p_combined 



#ggsave(filename = "SI_Fig_5", plot = p_combined, width = 180, height = 80, units = "mm", dpi = 900, device = cairo_pdf)














