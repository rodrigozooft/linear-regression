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

