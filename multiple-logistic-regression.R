# Get the actual responses from churn
actual_response <- churn$has_churned

# Get the predicted responses from the model
predicted_response <- round(fitted(mdl_churn_vs_both_inter))

# Get a table of these values
outcomes <- table(predicted_response, actual_response)

# Convert the table to a conf_mat object
confusion <- conf_mat(outcomes)

# See the result
confusion

# From previous step
actual_response <- churn$has_churned
predicted_response <- round(fitted(mdl_churn_vs_both_inter))
outcomes <- table(predicted_response, actual_response)
confusion <- conf_mat(outcomes)

# "Automatically" plot the confusion matrix
autoplot(confusion)

# Get summary metrics
summary(confusion, event_level = "second")

# From previous step
logistic_distn_cdf <- tibble(
  x = seq(-10, 10, 0.1),
  logistic_x = plogis(x),
  logistic_x_man = 1 / (1 + exp(-x))
)

# Using logistic_distn_cdf, plot logistic_x vs. x
ggplot(logistic_distn_cdf, aes(y = logistic_x, x = x)) +
  # Make it a line plot
  geom_line()

# From previous step
logistic_distn_inv_cdf <- tibble(
  p = seq(0.001, 0.999, 0.001),
  logit_p = qlogis(p),
  logit_p_man = log(p / (1 - p))
)

# Using logistic_distn_inv_cdf, plot logit_p vs. p
ggplot(logistic_distn_inv_cdf, aes(y = logit_p, x = p)) +
  # Make it a line plot
  geom_line()