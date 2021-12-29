# sparrow is in the workspace
summary(sparrow)

# Create the survived column
sparrow$survived <- sparrow$status == "Survived"

# Create the formula
(fmla <- as.formula("survived ~ total_length + weight + humerus"))

# Fit the logistic regression model
sparrow_model <- glm(fmla, data = sparrow, family = binomial)

# Call summary
summary(sparrow_model)

# Call glance
(perf <- glance(sparrow_model))

# Calculate pseudo-R-squared
(pseudoR2 <- 1 - perf$deviance / perf$null.deviance)

# sparrow is in the workspace
summary(sparrow)

# sparrow_model is in the workspace
summary(sparrow_model)

# Make predictions
sparrow$pred <- predict(sparrow_model, data = sparrow, type = "response")

# Look at gain curve
GainCurvePlot(sparrow, xvar = "pred", truthVar = "survived", "sparrow survival model")

# bikesJuly is in the workspace
str(bikesJuly)

# The outcome column
outcome 

# The inputs to use
vars 

# Create the formula string for bikes rented as a function of the inputs
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))

# Calculate the mean and variance of the outcome
(mean_bikes <- mean(bikesJuly$cnt))
(var_bikes <- var(bikesJuly$cnt))

# Fit the model
bike_model <- glm(fmla, data = bikesJuly, family = quasipoisson)

# Call glance
(perf <- glance(bike_model))

# Calculate pseudo-R-squared
(pseudoR2 <- 1 - perf$deviance/perf$null.deviance)

# bikesAugust is in the workspace
str(bikesAugust)

# bike_model is in the workspace
summary(bike_model)

# Make predictions on August data
bikesAugust$pred  <- predict(bike_model, type = "response", newdata = bikesAugust)

# Calculate the RMSE
bikesAugust %>% 
  mutate(residual = pred - cnt) %>%
  summarize(rmse  = sqrt(mean(residual ^ 2)))

# Plot predictions vs cnt (pred on x-axis)
ggplot(bikesAugust, aes(x = pred, y = cnt)) +
  geom_point() + 
  geom_abline(color = "darkblue")