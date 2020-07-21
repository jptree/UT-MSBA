#######  SETUP  ############
#If you have questions, ask Jason Petri!
# Make randomness....consistent.
rm(list = ls())
set.seed(1337)


# For this analysis, I will be attempting to predict the variable "quality"
# in the wine data set using combinations of dependent variables or singular
# uses of one dependent variable.


# Load white and red databases from .csv files
white_wine = read.csv('wine_white.csv')
red_wine = read.csv('wine_red.csv')

# Henceforth, white wine shall be referred to as '0' in statistical sense
white_wine$type = 0

# Henceforth, red wine shall be referred to as '1' in statistical sense
red_wine$type = 1

# Combines the white and red wine data frames together
red_white_df <- rbind(white_wine, red_wine)

# Create different models with different covariates for comparison
model_fixed_acidity = lm(red_white_df$quality ~ red_white_df$fixed.acidity)
model_density = lm(red_white_df$quality ~ red_white_df$density)
model_residual_sugar = lm(red_white_df$quality ~ red_white_df$residual.sugar)
model_all = lm(red_white_df$quality ~ red_white_df$fixed.acidity +
                 red_white_df$volatile.acidity +
                 red_white_df$citric.acid +
                 red_white_df$residual.sugar +
                 red_white_df$chlorides +
                 red_white_df$free.sulfur.dioxide +
                 red_white_df$total.sulfur.dioxide +
                 red_white_df$density +
                 red_white_df$pH +
                 red_white_df$sulphates +
                 red_white_df$alcohol +
                 red_white_df$type)

# Show summary of the models
summary(model_fixed_acidity)
summary(model_density)
summary(model_residual_sugar)
summary(model_all)

# Show scatterplot for each model and ordinary least squares
x_variable = red_white_df$fixed.acidity
model = model_fixed_acidity

plot(y = red_white_df$quality, x = x_variable,
     pch = 19,
     xlab = "acidity",
     ylab = "Quality")
abline(model$coef[1], model$coef[2],
       col = 2,
       lwd = 2)
