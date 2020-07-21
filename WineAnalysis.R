rm(list = ls())
#######  SETUP  ############
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
clustering_df <- red_wine[c('alcohol', 'quality')]

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





######## K-FOLDS CROSS VALIDATION #########
library(kknn)

k_folds_cross_validation__KNN <- function(folds, df, k) {
  
  rows_in_df = dim(df)[1]
  size_of_fold = round(rows_in_df / folds, 0)
  out_MSE = matrix(0,
                   nrow = folds,
                   ncol = length(k))
  used = NULL
  set = 1:rows_in_df
  
  
  for (j in 1:folds) {
    
    if (size_of_fold < length(set)) {
      val = sample(set, size = size_of_fold)
    }
    
    if (size_of_fold >= length(set)) {
      val = set
    }
    
    
    train = df[-val,]
    test = df[val,]
    
    for (i in k) {
      
      current_model = kknn(Y ~.,
                           train = train,
                           test = test,
                           k = i,
                           kernel = 'rectangular')
      mse_current_model = mean((test[,1]-current_model$fitted)^2)
      out_MSE[j, i] = mse_current_model
    }
    
    used = union(used, val)
    set = (1:rows_in_df)[-used]
    cat(j, "folds out of", folds, '\n')
    
  }
  
  mean_MSE = apply(out_MSE, 2, mean)
  par(mfrow = c(1,1))
  
  plot(log(1/k),sqrt(mean_MSE), #the values
       xlab="Complexity (log(1/k))",
       ylab="out-of-sample RMSE",
       main = "California Housing (knn)",
       col=4, #Color of line
       lwd=2, #Line width
       type="l", #Type of graph = line
       cex.lab=1.2) #Size of labs
  
  best = which.min(mean_MSE)
  
  
  text(log(1/k[best]),sqrt(mean_MSE[best])+0.01, #Coordinates
       paste("k=",k[best]),#The actual text
       col=2, #Color of the text
       cex=1.2) #Size of the text
  #text(log(1/100)+1,sqrt(mean_MSE[100]), paste("k=", max(k)))
  text(log(1/100)+1,sqrt(mean_MSE[100]), "k=100")
  text(log(1/1)-0.5,sqrt(mean_MSE[1])+0.001,"k=1")
  
  
  
  
  return('yeet')
  
}


ca <- read.csv("C:/Users/jason/Desktop/Class/CAhousing.csv")
indicies = sample(1:dim(ca)[1],
                  size = 1000,
                  replace = FALSE)
Y = log(ca$medianHouseValue)[indicies]
caData = ca[indicies,]

rooms = scale(caData$totalRooms) * sd(caData$latitude)
income = scale(caData$medianIncome) * sd(caData$latitude)
age = scale(caData$housingMedianAge) * sd(caData$latitude)


df_data <- data.frame(Y, caData$latitude, caData$longitude, rooms, income, age)

k_folds_cross_validation__KNN(10, df_data, 1:100)







####### MULTIPLE LOGISTIC REGRESSION #######

x_variable = red_white_df$citric.acid

plot(red_white_df$type~x_variable, #Formula
     data=red_white_df, #Data
     col=c('lightblue','orange'), #Color of boxplots
     cex.axis=1.2, #Size of axis text
     cex.lab=1.2) #Size of lab text

# Plotting the Logistic Function
z = seq(from=-5,to=5,length.out=1000) #A vector of values for an interval
Fz = exp(z)/(1+exp(z)) #Applying the logistic link
plot(z,Fz, #The data
     type='l', #Type = line
     col='blue', #Color of line
     lwd=2.5, #Line width
     xlab=expression(eta), #Expression allows you to write greek letters
     ylab='F',
     cex.lab=1.3) #Size of lab

# Logistic link function
f_logit = function(x) {
  exp(x) / (1 + exp(x))
}

# Fitting general logistic model
glm.fit = glm(red_white_df$type~x_variable,
              data = red_white_df,
              family = binomial)
eta = predict(glm.fit)
pyx = predict(glm.fit, type = 'response')

par(mfrow = c(3, 1)) # plot window: 3 rows, 1 column

n = nrow(red_white_df)
sample_indices = sample(1:n,
                        size = 50,
                        replace = FALSE)
plot(x_variable, eta)
points(x_variable[sample_indices], 
       eta[sample_indices], 
       pch = 16, 
       col = 'red')
title(main = 'eta vs. F(eta)', cex.main = 1.5)


