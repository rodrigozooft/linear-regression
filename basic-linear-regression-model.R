# unemployment is loaded in the workspace
summary(unemployment)

# Define a formula to express female_unemployment as a function of male_unemployment
fmla <- female_unemployment ~ male_unemployment

# Print it
fmla

# Use the formula to fit a model: unemployment_model
unemployment_model <- lm(formula = fmla, data = unemployment)

# Print it
unemployment_model

# broom and sigr are already loaded in your workspace
# Print unemployment_model
unemployment_model

# Call summary() on unemployment_model to get more details
summary(unemployment_model)

# Call glance() on unemployment_model to see the details in a tidier form
glance(unemployment_model)

# Call wrapFTest() on unemployment_model to see the most relevant details
wrapFTest(unemployment_model)

# unemployment is in your workspace
summary(unemployment)

# newrates is in your workspace
newrates

# Predict female unemployment in the unemployment data set
unemployment$prediction <-  predict(unemployment_model, unemployment)

# load the ggplot2 package
library(ggplot2)

# Make a plot to compare predictions to actual (prediction on x axis). 
ggplot(unemployment, aes(x = prediction, y = female_unemployment)) + 
  geom_point() +
  geom_abline(color = "blue")

# Predict female unemployment rate when male unemployment is 5%
pred <- predict(unemployment_model, newrates)
# Print it
pred

# bloodpressure is in the workspace
summary(bloodpressure)

# Create the formula and print it
fmla <- blood_pressure ~ age + weight
fmla

# Fit the model: bloodpressure_model
bloodpressure_model <- lm(formula = fmla, data = bloodpressure)

# Print bloodpressure_model and call summary() 
bloodpressure_model
summary(bloodpressure_model)

# bloodpressure is in your workspace
summary(bloodpressure)

# bloodpressure_model is in your workspace
bloodpressure_model

# predict blood pressure using bloodpressure_model :prediction
bloodpressure$prediction <- predict(bloodpressure_model, bloodpressure)

# plot the results
ggplot(bloodpressure, aes(x = prediction, y = blood_pressure)) + 
    geom_point() +
    geom_abline(color = "blue")

# From previous step
unemployment$predictions <- predict(unemployment_model)

# Calculate residuals
unemployment$residuals <- unemployment$female_unemployment - unemployment$predictions

# Fill in the blanks to plot predictions (on x-axis) versus the residuals
ggplot(unemployment, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("residuals vs. linear model prediction")

# unemployment is in the workspace (with predictions)
summary(unemployment)

# unemployment_model is in the workspace
summary(unemployment_model)

# Load the package WVPlots
library(WVPlots)

# Plot the Gain Curve
GainCurvePlot(unemployment, "predictions", "female_unemployment", "Unemployment model")

# unemployment is in the workspace
summary(unemployment)

# For convenience put the residuals in the variable res
res <- unemployment$residuals

# Calculate RMSE, assign it to the variable rmse and print it
(rmse <- sqrt(mean(res ^ 2)))

# Calculate the standard deviation of female_unemployment and print it
(sd_unemployment <- sd(unemployment$female_unemployment))

# unemployment is in your workspace
summary(unemployment)

# unemployment_model is in the workspace
summary(unemployment_model)

# Calculate mean female_unemployment: fe_mean. Print it
(fe_mean <- mean(unemployment$female_unemployment))

# Calculate total sum of squares: tss. Print it
(tss <- sum((unemployment$female_unemployment - fe_mean)^2))

# Calculate residual sum of squares: rss. Print it
(rss <- sum(unemployment$residuals ^ 2))

# Calculate R-squared: rsq. Print it. Is it a good fit?
(rsq <- 1 - rss / tss)

# Get R-squared from glance. Print it
(rsq_glance <- glance(unemployment_model)$r.squared)

# unemployment is in your workspace
summary(unemployment)

# unemployment_model is in the workspace
summary(unemployment_model)

# Get the correlation between the prediction and true outcome: rho and print it
(rho <- cor(unemployment$predictions, unemployment$female_unemployment))

# Square rho: rho2 and print it
(rho2 <- rho ^ 2)

# Get R-squared from glance and print it
(rsq_glance <- glance(unemployment_model)$r.squared)

# mpg is in the workspace
summary(mpg)
dim(mpg)

# Use nrow to get the number of rows in mpg (N) and print it
(N <- nrow(mpg))

# Calculate how many rows 75% of N should be and print it
# Hint: use round() to get an integer
(target <- round(0.75 * N))

# Create the vector of N uniform random variables: gp
gp <- runif(N)

# Use gp to create the training set: mpg_train (75% of data) and mpg_test (25% of data)
mpg_train <- mpg[gp < 0.75, ]
mpg_test <- mpg[gp >=  0.75, ]

# Use nrow() to examine mpg_train and mpg_test
nrow(mpg_train)
nrow(mpg_test)