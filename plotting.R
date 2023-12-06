# Linear Regression Plot to show each sex separately.
plot_regression_figure_1 <- function(culmen_data) {
  penguin_linear_model <- lm(culmen_depth_mm ~ culmen_length_mm + sex, data = culmen_data)
  
  culmen_data %>% 
    ggplot(aes(x = culmen_length_mm, y = culmen_depth_mm, colour = sex )) +
    geom_point() + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +  # Remove confidence interval
    labs(x = "Culmen Length (mm)", y = "Culmen Depth (mm)") +
    ggtitle("A linear regression plot to show the relationship between culmen length, depth and sex across Antarctica penguins") +
    annotate("text", x = 35, y = 14.5, label = paste("R-squared =", round(summary(penguin_linear_model)$r.squared, 3)), size = 3) +
    annotate("text", x = 35, y = 14, label = paste("p-value = ", formatC(summary(penguin_linear_model)$coefficients[3, "Pr(>|t|)"], digits = 3, format = "f")), size = 3) +
    theme_bw()
}

# Linear regression plot to show the overall regression line.
plot_regression_figure_2 <- function(culmen_data) {
  penguin_linear_model <- lm(culmen_depth_mm ~ culmen_length_mm + sex, data = culmen_data)
  
  culmen_data %>% 
    ggplot(aes(x = culmen_length_mm, y = culmen_depth_mm, colour = sex)) +
    geom_point() + 
    geom_smooth(method = "lm", colour = "blue", formula = y ~ x, se = FALSE) +  # Remove confidence interval
    labs(x = "Culmen Length (mm)", y = "Culmen Depth (mm)") +
    ggtitle("A linear regression plot to show the relationship between culmen length, depth and sex across Antarctica penguins") +
    annotate("text", x = 35, y = 14.5, label = paste("R-squared =", round(summary(penguin_linear_model)$r.squared, 3)), size = 3) +
    annotate("text", x = 35, y = 14, label = paste("p-value = ", formatC(summary(penguin_linear_model)$coefficients[3, "Pr(>|t|)"], digits = 3, format = "f")), size = 3) +
    theme_bw()
}



