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


