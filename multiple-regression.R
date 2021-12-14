# Working with multiple regression and parallel regression
# Fit a linear regr'n of price_twd_msq vs. n_convenience 
# plus house_age_years, no intercept
mdl_price_vs_both <- lm(formula = price_twd_msq ~ n_convenience + house_age_years + 0, data = taiwan_real_estate)

# See the result
mdl_price_vs_both

# Using taiwan_real_estate, plot price_twd_msq vs. n_convenience
# colored by house_age_years
ggplot(taiwan_real_estate, aes(y = price_twd_msq, x = n_convenience, color = house_age_years)) +
  # Add a point layer
  geom_point() +
  # Add parallel slopes, no ribbon
  geom_parallel_slopes(se = FALSE)