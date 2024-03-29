# Applying a linear regression model using interactions

# Model price vs both with an interaction using "colon" syntax
lm(price_twd_msq ~ n_convenience + house_age_years + n_convenience:house_age_years, data = taiwan_real_estate)

# From previous step
explanatory_data <- expand_grid(
  n_convenience = 0:10,
  house_age_years = unique(taiwan_real_estate$house_age_years)
)
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_both_inter, explanatory_data)
  )

# 
taiwan_real_estate %>%
  ggplot(aes(x = n_convenience, y = price_twd_msq, color = house_age_years)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_point(
    data = prediction_data,
    size = 5,
    shape = 15
  )

# From previous step
coeffs <- coefficients(mdl_price_vs_both_inter)
intercept_0_15 <- coeffs[1]
intercept_15_30 <- coeffs[2]
intercept_30_45 <- coeffs[3]
slope_0_15 <- coeffs[4]
slope_15_30 <- coeffs[5]
slope_30_45 <- coeffs[6]

prediction_data <- explanatory_data %>% 
  mutate(
    # Consider the 3 cases to choose the price
    price_twd_msq = case_when(
      house_age_years == "0 to 15" ~ intercept_0_15 + slope_0_15 * n_convenience,
      house_age_years == "15 to 30" ~ intercept_15_30 + slope_15_30 * n_convenience,
      house_age_years == "30 to 45" ~ intercept_30_45 + slope_30_45 * n_convenience
    )
  )

# See the result
prediction_data

# Using auctions, plot price vs. opening bid colored by
# auction type as a scatter plot with linear regr'n trend lines
auctions %>%
    ggplot(aes(x = openbid, y = price, color = auction_type)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE)

# With taiwan_real_estate, draw a 3D scatter plot of
# no. of conv. stores, sqrt dist to MRT, and price
scatter3D(x = taiwan_real_estate$n_convenience, y = sqrt(taiwan_real_estate$dist_to_mrt_m), z = taiwan_real_estate$price_twd_msq)

# Using taiwan_real_estate, plot sqrt dist to MRT vs. 
# no. of conv stores, colored by price
ggplot(taiwan_real_estate, aes(x = n_convenience, y = sqrt(dist_to_mrt_m), color = price_twd_msq)) + 
  # Make it a scatter plot
  geom_point() +
  # Use the continuous viridis plasma color scale
  scale_color_viridis_c(option = "plasma")

# From previous steps
mdl_price_vs_conv_dist <- lm(price_twd_msq ~ n_convenience + sqrt(dist_to_mrt_m), data = taiwan_real_estate)
explanatory_data <- expand_grid(n_convenience = 0:10, dist_to_mrt_m = seq(0, 80, 10) ^ 2)
prediction_data <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_price_vs_conv_dist, explanatory_data))

# Add predictions to plot
ggplot(
  taiwan_real_estate, 
  aes(n_convenience, sqrt(dist_to_mrt_m), color = price_twd_msq)
) + 
  geom_point() +
  scale_color_viridis_c(option = "plasma")+
  # Add prediction points colored yellow, size 3
  geom_point(
    data = prediction_data,
    color = "yellow",
    size = 3
  )

# From previous steps
mdl_price_vs_conv_dist <- lm(price_twd_msq ~ n_convenience * sqrt(dist_to_mrt_m), data = taiwan_real_estate)
explanatory_data <- expand_grid(n_convenience = 0:10, dist_to_mrt_m = seq(0, 80, 10) ^ 2)
prediction_data <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_price_vs_conv_dist, explanatory_data))

# Add predictions to plot
ggplot(
  taiwan_real_estate, 
  aes(n_convenience, sqrt(dist_to_mrt_m), color = price_twd_msq)
) + 
  geom_point() +
  scale_color_viridis_c(option = "plasma") +
  # Add prediction points colored yellow, size 3
  geom_point(
    data = prediction_data,
    color = "yellow",
    size = 3
  )
# Using taiwan_real_estate, no. of conv. stores vs. sqrt of
# dist. to MRT, colored by plot house price
ggplot(taiwan_real_estate, aes(y = n_convenience, x = sqrt(dist_to_mrt_m), color = price_twd_msq)) +
  # Make it a scatter plot
  geom_point() +
  # Use the continuous viridis plasma color scale
  scale_color_viridis_c(optioin = "plasma") +
  # Facet, wrapped by house age
  facet_wrap(~ house_age_years)

# Model price vs. sqrt dist. to MRT station, no. of conv.
# stores & house age, no global intercept, 2-way interactions
mdl_price_vs_all_2_way_inter <- lm(price_twd_msq ~ (sqrt(dist_to_mrt_m) + n_convenience + house_age_years) ^ 2 + 0, data = taiwan_real_estate)


# See the result
mdl_price_vs_all_2_way_inter

# From previous step
explanatory_data <- expand_grid(
  dist_to_mrt_m = seq(0, 80, 10) ^ 2,
  n_convenience = 0:10,
  house_age_years = unique(taiwan_real_estate$house_age_years)
)
prediction_data <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_price_vs_all_3_way_inter, explanatory_data))

# Extend the plot
ggplot(
  taiwan_real_estate, 
  aes(sqrt(dist_to_mrt_m), n_convenience, color = price_twd_msq)
) +
  geom_point() +
  scale_color_viridis_c(option = "plasma") +
  facet_wrap(vars(house_age_years)) +
  # Add points from prediction data, size 3, shape 15
  geom_point(
    data = prediction_data,
    size = 3,
    shape = 15
  )

  calc_sum_of_squares <- function(coeffs) {
  # Get the intercept coeff
  intercept <- coeffs[1]

  # Get the slope coeff
  slope <- coeffs[2]

  # Calculate the predicted y values
  y_pred <- intercept + slope * x_actual

  # Calculate the differences between actual and predicted
  y_diff <- y_actual - y_pred

  # Calculate the sum of squares
  sum(y_diff ^ 2)
}

# From previous step
calc_sum_of_squares <- function(coeffs) {
  intercept <- coeffs[1]
  slope <- coeffs[2]
  y_pred <- intercept + slope * x_actual
  y_diff <- y_actual - y_pred
  sum(y_diff ^ 2)
}

# Optimize the metric
optim(
  # Initially guess 0 intercept and 0 slope
  par = c(intercept = 0, slope = 0), 
  # Use calc_sum_of_squares as the optimization fn
  fn = calc_sum_of_squares
)

# Compare the coefficients to those calculated by lm()
lm(price_twd_msq ~ n_convenience, data = taiwan_real_estate)

# From previous steps
explanatory_data <- expand_grid(
  time_since_first_purchase = seq(-2, 4, 0.1),
  time_since_last_purchase = seq(-1, 6, 0.1)
)
prediction_data <- explanatory_data %>% 
  mutate(
    has_churned = predict(mdl_churn_vs_both_inter, explanatory_data, type = "response")
  )

# Extend the plot
ggplot(
  churn, 
  aes(time_since_first_purchase, time_since_last_purchase, color = has_churned)
) +
  geom_point(alpha = 0.5) +
  scale_color_gradient2(midpoint = 0.5) +
  theme_bw() +
  # Add points from prediction_data with size 3 and shape 15
  geom_point(
    data = prediction_data,
    size = 3,
    shape = 15
  )