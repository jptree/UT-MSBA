
#######  SETUP  ############
#test
# Make randomness....consistent.
set.seed(1337)

# Load white and red databases from .csv files
white_wine = read.csv('wine_white.csv')
red_wine = read.csv('wine_red.csv')

# Henceforth, white wine shall be referred to as '0' in statistical sense
white_wine$type = 0

# Henceforth, red wine shall be referred to as '1' in statistical sense
red_wine$type = 1

# Combines the white and red wine data frames together
red_white_df <- rbind(white_wine, red_wine)


########  K-MEANS CLUSTERING  ###########
library(factoextra)
library(gridExtra)


# This is the data frame that will be used to cluster the wine observations
# The columns have arbitrarily been picked
clustering_df <- white_wine[c('alcohol', 'pH', 'density', 'sulphates')]

# You must scale your data into the same range of values for this model
clustering_df <- scale(clustering_df)

# This fits the model using specified quantity of centers, i.e. the groupings
k2 <- kmeans(clustering_df, centers = 2, nstart = 25)
k3 <- kmeans(clustering_df, centers = 3, nstart = 25)
k4 <- kmeans(clustering_df, centers = 4, nstart = 25)
k5 <- kmeans(clustering_df, centers = 5, nstart = 25)

# This produces plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = clustering_df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = clustering_df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = clustering_df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = clustering_df) + ggtitle("k = 5")

# Puts all the plots together into one image.
grid.arrange(p1, p2, p3, p4, nrow = 2)



