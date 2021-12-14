# working with logistic regression

# Redraw the plot with time_since_first_purchase
churn %>%
    ggplot(aes(x = time_since_first_purchase)) + 
    geom_histogram(binwidth = 0.25) + 
    facet_grid(rows = vars(has_churned))

# Using churn plot has_churned vs. time_since_first_purchase
  ggplot(churn, aes(x = time_since_first_purchase, y = has_churned)) +
  # Make it a scatter plot
  geom_point() +
  # Add an lm trend line, no std error ribbon, colored red
  geom_smooth(method = "lm", color = "red", se = FALSE)


ggplot(churn, aes(time_since_first_purchase, has_churned)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  # Add a glm trend line, no std error ribbon, binomial family
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = binomial))

# Fit a logistic regression of churn vs. 
# length of relationship using the churn dataset
mdl_churn_vs_relationship <- glm(has_churned ~ time_since_first_purchase, family = binomial, data = churn)

# See the result
mdl_churn_vs_relationship

# From previous step
prediction_data <- explanatory_data %>% 
  mutate(   
    has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response")
  )

# Update the plot
plt_churn_vs_relationship +
  # Add points from prediction_data, colored yellow, size 2
  geom_point(
    aes(y = has_churned),
    data = prediction_data,
    color = "yellow",
    size = 2
  )

# From previous step
prediction_data <- explanatory_data %>% 
  mutate(   
    has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response"),
    most_likely_outcome = round(has_churned)
  )

# Update the plot
plt_churn_vs_relationship +
  # Add most likely outcome points from prediction_data, 
  # colored yellow, size 2
  geom_point(
    aes(y = most_likely_outcome),
    data = prediction_data,
    color = "yellow",
    size = 2
  )
