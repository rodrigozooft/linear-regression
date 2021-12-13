# working with logistic regression

# Redraw the plot with time_since_first_purchase
churn %>%
    ggplot(aes(x = time_since_first_purchase)) + 
    geom_histogram(binwidth = 0.25) + 
    facet_grid(rows = vars(has_churned))