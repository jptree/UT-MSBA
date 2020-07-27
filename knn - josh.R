setwd("C:/Users/Admin/Documents/GitHub/UT-MSBA")
rm(list = ls())
set.seed(1337)

# Load white and red databases from .csv files
white_wine = read.csv('wine_white.csv')
red_wine = read.csv('wine_red.csv')
# Henceforth, white wine shall be referred to as '0' in statistical sense
white_wine$type = 0
mean(red_white_df$quality)
# Henceforth, red wine shall be referred to as '1' in statistical sense
red_wine$type = 1

# Combines the white and red wine data frames together
red_white_df <- rbind(white_wine, red_wine)

library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools
library(kknn) ## knn library

install.packages("scales") #package for scaling
library(scales)

#rescaling
red_white_df$alcohol.rescaled <- rescale(red_white_df$alcohol) 
red_white_df$volatile.acidity.rescaled <- rescale(red_white_df$volatile.acidity) 
red_white_df$citric.acid.rescaled <- rescale(red_white_df$citric.acid) 
red_white_df$fixed.acidity.rescaled <- rescale(red_white_df$fixed.acidity) 
red_white_df$residual.sugar.rescaled <- rescale(red_white_df$residual.sugar) 
red_white_df$chlorides.rescaled <- rescale(red_white_df$chlorides) 
red_white_df$free.sulfur.dioxide.rescaled <- rescale(red_white_df$free.sulfur.dioxide) 
red_white_df$total.sulfur.dioxide.rescaled <- rescale(red_white_df$total.sulfur.dioxide) 
red_white_df$density.rescaled <- rescale(red_white_df$density) 
red_white_df$pH.rescaled <- rescale(red_white_df$pH) 
red_white_df$quality.rescaled <- rescale(red_white_df$quality) 
red_white_df$sulphates.rescaled <- rescale(red_white_df$sulphates)

#red_white_df$quality.rescaled[red_white_df$quality.rescaled > 0.7 ] <- 50 

plot(red_white_df$alcohol.rescaled, red_white_df$quality.rescaled, pch=19)
plot(red_white_df$sulphates.rescaled, red_white_df$quality.rescaled, pch=19)
plot(red_white_df$residual.sugar.rescaled, red_white_df$quality.rescaled, pch=19)


#Organizing the data in a training df
train = data.frame(red_white_df$alcohol,red_white_df$quality)

#Now for the test df
test = data.frame(red_white_df$alcohol,red_white_df$quality)
ind = order(test[,1]) #saving the indexes of the first column ordered
test = test[ind,] #Then rewriting the test df with the first column ordered

#Creating the object MSE to be used inside the for loop
MSE = NULL

#Number of neighbors for different models
kk = c(2,10,50,100,150,200,250,300,400,505)

for(i in kk){
  #Storing the knn model
  near = kknn(red_white_df$quality ~ red_white_df$alcohol, #The formula
              train = train, #The train matrix/df
              test = test, #The test matrix/df
              k=i, #The number of neighbors
              kernel = "rectangular") #Type of kernel (see options in the help section)
  
  #Calculating the MSE of the current model
  aux = mean((test[,2]-near$fitted)^2)

  #Store the MSE of the current model in the MSE object
  MSE = c(MSE,aux)
  
  #Plot a scatterplot
  plot(red_white_df$alcohol,red_white_df$quality,xlab='alcohol', ylab='quality',main=paste("k=",i),pch=19,cex=0.8,col="darkgray")
  
  #And include the fitted values of the current model
  lines(test[,1],near$fitted,col=2,lwd=2)
  
  #Read whatever the user input in the console window.
  #Since we are not storing the readline function in any object
  #then we are simply stopping the for loop until the user press enter at the terminal
  readline("Press [enter] to continue")
}
MSE=MSE[-11] #copied the last loop twice for some reason

#Complexity x RMSE graph
plot(log(1/kk),sqrt(MSE), #the values of the graph
     type="b", #Both points and lines
     xlab="Complexity (log(1/k))",
     col="blue", #Color of the line
     ylab="RMSE",
     lwd=2, #line width
     cex.lab=1.2) #Size of lab text

#Inclusing text at specific coordinates of the graph
text(log(1/kk[1]),sqrt(MSE[1])+0.3, #coordinates
     paste("k=",kk[1]), #the actual text
     col=2, #Color of the text
     cex=1.2) #Size of the text
text(log(1/kk[2])+0.4,sqrt(MSE[2]),paste("k=",kk[2]),col=2,cex=1.2)
text(log(1/kk[5])+0.4,sqrt(MSE[5]),paste("k=",kk[5]),col=2,cex=1.2)
text(log(1/kk[10])+0.4,sqrt(MSE[10]),paste("k=",kk[10]),col=2,cex=1.2)

###########Out of Sample Prediction############

#Train and test datasets
train = data.frame(red_white_df$alcohol,red_white_df$quality)
test = data.frame(red_white_df$alcohol,red_white_df$quality)

#Sample (in this case with uniform distribution)
tr = sample(1:6497, #The values that will be sampled
            size = 4997, #The size of the sample
            replace = FALSE) #without replacement

train = train[tr,] #the rows of train will be the ones sampled
test = test[-tr,] #and test will be everything else (thus, out-of-sample)

#Create the object for storing the MSE inside the for
out_MSE = NULL

#Number of neighbors for different models
kk <- c(100,150,200,250,300,350,400,450,500,505)

for(i in kk){
  #The current model
  near = kknn(red_white_df$quality ~ red_white_df$alcohol, #The formula
              train = train, #The train matrix/df
              test = test, #The test matrix/df
              k=i, #The number of neighbors
              kernel = "rectangular") #Type of kernel (see options in the help section)
  
  #Calculating the MSE for the current model
  aux = mean((test[,2]-near$fitted)^2)
  
  #Concatenate the MSE in the out_MSE object
  out_MSE = c(out_MSE,aux)
}
warnings()
#Find which position of out_MSE has the lowest MSE
best = which.min(out_MSE)

#Complexity x RMSE graph
plot(log(1/kk),sqrt(out_MSE), #Values of the graph
     xlab="Complexity (log(1/k))",
     ylab="out-of-sample RMSE",
     col=4, #Color
     lwd=2, #line width
     type="l", #Type of graph = line
     cex.lab=1.2) #Size of lab text

#Inclusing text at specific coordinates of the graph
text(log(1/kk[best]),sqrt(out_MSE[best])+0.3, #coordinates
     paste("k=",kk[best]), #the actual text
     col=2, #color
     cex=1.2) #Size of text
text(log(1/2),sqrt(out_MSE[2]),paste("k=",2),col=2,cex=1.2)
text(log(1/354)+0.4,sqrt(out_MSE[345]),paste("k=",345),col=2,cex=1.2)


#Look at the fit of the best model
near = kknn(red_white_df$quality ~ red_white_df$alcohol, #The formula
            train = train, #The train matrix/df
            test = test, #The test matrix/df
            k=kk[best], #Number of neighbors
            kernel = "rectangular") #Type of kernel (see help for more)

#saving the indexes of the first column ordered
ind = order(test[,1])
#scatterplot
plot(red_white_df$alcohol,red_white_df$quality, #data
     main=paste("k=",kk[best]), #Title of the graph
     pch=19, #Type of point
     cex=0.8, #Size of points
     col="darkgray") #Color of points

lines(test[ind,1],near$fitted[ind], #Data ordered accoring to the indices
      col=2, #color of the line
      lwd=2) #line width

out_MSE
min(out_MSE)

######K-FOLD######

#Data frame of the data
data_df = data.frame(lstat,medv)

#Define the number of folds
kcv = 10 #In this case, 10-fold cross validation
#This k has nothing to do with the k from knn

#Size of the fold (which is the number of elements in the test matrix)
n0 = round(n/kcv, #Number of observations in the fold
           0) #Rounding with 0 decimals

#Number of neighbors for different models
kk <- 1:450

#MSE matrix
out_MSE = matrix(0, #matrix filled with zeroes
                 nrow = kcv, #number of rows
                 ncol = length(kk)) #number of columns

#Vector of indices that have already been used inside the for
used = NULL

#The set of indices not used (will be updated removing the used)
set = 1:n

for(j in 1:kcv){
  
  if(n0<length(set)){ #If the set of 'not used' is > than the size of the fold
    val = sample(set, size = n0) #then sample indices from the set
  }
  
  if(n0>=length(set)){ #If the set of 'not used' is <= than the size of the fold
    val=set #then use all of the remaining indices as the sample
  }
  
  #Create the train and test matrices
  train_i = data_df[-val,] #Every observation except the ones whose indices were sampled
  test_i = data_df[val,] #The observations whose indices sampled
  
  for(i in kk){
    
    #The current model
    near = kknn(red_white_df$quality ~ red_white_df$alcohol, #The formula
                train = train_i, #The train matrix/df
                test = test_i, #The test matrix/df
                k=i, #Number of neighbors
                kernel = "rectangular") #Type of kernel (see help for more)
    
    #Calculating the MSE of current model
    aux = mean((test_i[,2]-near$fitted)^2)
    
    #Store the current MSE
    out_MSE[j,i] = aux
  }
  
  #The union of the indices used currently and previously
  used = union(used,val)
  
  #The set of indices not used is updated
  set = (1:n)[-used]
  
  #Printing on the console the information that you want
  #Useful to keep track of the progress of your loop
  cat(j,"folds out of",kcv,'\n')
}

warnings()
#Calculate the mean of MSE for each k
mMSE = apply(out_MSE, #Receive a matrix
             2, #Takes its columns (it would take its rows if this argument was 1)
             mean) #And for each column, calculate the mean

#Complexity x RMSE graph
plot(log(1/kk),sqrt(mMSE),
     xlab="Complexity (log(1/k))",
     ylab="out-of-sample RMSE",
     col=4, #Color of line
     lwd=2, #Line width
     type="l", #Type of graph = line
     cex.lab=1.2, #Size of labs
     main=paste("kfold(",kcv,")")) #Title of the graph

#Find the index of the minimum value of mMSE
best = which.min(mMSE)

#Inclusing text at specific coordinates of the graph
text(log(1/kk[best])+1,sqrt(mMSE[best])+0.06, #Coordinates
     paste("k=",kk[best]), #The actual text
     col=2, #Color of the text
     cex=1.2) #Size of the text
text(log(1/200)+1.2,sqrt(mMSE[200])+0.08,paste("k=",200),col=2,cex=1.2)
text(log(1/100)+1.5,sqrt(mMSE[100])+0.11,paste("k=",100),col=2,cex=1.2)

sqrtmMSE=sqrt(mMSE)
min(sqrtmMSE)
best
sqrtmMSE
