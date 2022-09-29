# Load white and red databases from .csv files
white_wine = read.csv('wine_white.csv')
red_wine = read.csv('wine_red.csv')

# Henceforth, white wine shall be referred to as '0' in statistical sense
white_wine$type = 0

# Henceforth, red wine shall be referred to as '1' in statistical sense
red_wine$type = 1

# Combines the white and red wine data frames together
red_white_df <- rbind(white_wine, red_wine)


summary(red_white_df)
#plot(red_white_df)
hist(red_white_df$quality, main = 'Quality of Wines', xlab = 'Quality', col = 'blue')

hist(red_white_df$type, main = 'Frequency of Wine Type', xlab = 'Type', col = 'blue')
barplot(red_white_df$type, main = 'Frequency of Wine Type', xlab = 'Type', col = 'blue')

plot(red_white_df[c(2, 6, 10, 11, 12)])
plot(red_white_df[c(6, 11, 2, 10, 12)])


