##########################################################################
############################ Tree analysis ###############################
########################################################################## 

#The set up was taken from WineAnalysis.R
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

#Attach dataframe so variables can be referenced as objects.
attach(red_white_df)

#Create train-value-test dataframes.
#Train stands for the training data
#Value stands for value. Most of the time it will be used with the training data
# in a df called trainval
#Test is the test data.

#First find how many rows are in the dataset.
n = nrow(red_white_df)

#Then determine a split. I choose 50-25-25
# or 75-25.
#Floor will round the value down in case of an odd number of rows.
n1 = floor(n/2)
n2 = floor(n/4)
#Most of the time n3 will be n/4 but with odd rows, it'll be n-n1-n2 or (n/4)+2
n3 = n-n1-n2

#Take sample starting from 1 to the number of rows, sample out the number of rows.
ii = sample(1:n,n)

#Go through the df ii. Use the observations for 1:n1 indices.
wine_train_df=red_white_df[ii[1:n1],]
#Go through the df ii. Use the observations for n1+1:n2 indices.
wine_value_df = red_white_df[ii[n1+1:n2],]
#Go through the df ii. Use the observations for n1++n2+1:n3 indices.
wine_test_df = red_white_df[ii[n1+n2+1:n3],]

#Combine the train and value df for potential future usage.
wine_train_value_df = rbind(wine_train_df,wine_value_df)

##########################################################################
###################### Simple Tree Model #################################
########################################################################## 

#The simple tree model is "greedy". We make a really really big tree and then prune it donw.

#Create the tree model using tree function from library trees
tree_wine = tree(quality~., #Predict quality using all the variables (~.).
                     data = wine_train_value_df, #use the train value dataframe.
                     mindev=.0001) #The tree will split at this minimum deviation.
# It is quite small because we want the tree to be big.

#Let's see how big the tree is.
cat('The tree has this many leaf nodes: \n')
print(length(unique(tree_wine$where))) #Number of leaf nodes

#use summary to determine what variables are used in the tree model.
summary(tree_wine)

#Seems like all but one variable was used for the tree.
#now that's a big tree.

#Let's determine how many nodes are actually used for the pruned tree.
#trees library has a built in cross validation function
cv_wine = cv.tree(tree_wine) 
#This function writes a component called dev. It is the sum of the deviation components from each fit.

#plot the results
plot(cv_wine$size ,cv_wine$dev ,type='b', ylim = range(2822,2830), xlim = range(1,20))
#The range is limited to zoom in on the values that are important.
#Basically shrink it down to 5 or more nodes.
#however the deviation suggests that the model here is not ideal.

#Regardless, let's try the pruned tree.
prune_wine = prune.tree(tree_wine, #Take tree model
                            best = 5) #Want to have only 5 nodes.
#plot results
plot(prune_wine)
text(prune_wine, pretty = 0)

#Getting the MSE
#First use the model to obtain predicted values
predict_quality_prune = predict(prune_wine, #Use the pruned model
                              newdata = wine_test_df) #Use the test df which has remaining data.
#find the root mean square error
sqrt(mean((predict_quality_prune - wine_test_df$quality)^2))
#Wait this (about 0.01895281) root mean square error's kinda good.
#This does make sense when looking at the variables earlier.
#Most of the wine's are pretty similar since they're all Portuguse wine.
#I'd imagine red or white doesn't make too much of a difference.

#Look at variable importance:
#In contrast, the pruned model only ends up using three variables.
summary(prune_wine)


##########################################################################
###################### Bagging Random Forests ############################
########################################################################## 

#excuse the cheeky section title.

#Now let's try out the random forests.
#First I want to cut to the chase and just select the best random forest model.
#The bagging model would be a special case of random forests where all m variables
# are selected for the tree.
#As it turns out trying to determine the best m is kinda computationally expensive.
#hence the warning.

# ###################     WARNING WILL TAKE A WHILE TO RUN###################################
# #What I'm doing here is very computationally expensive.
# #It will take a while to run.
# #For that reason I've commentated this section out.
# #If you want to try running it uncomment out this section.
# #I've outputted the main important thing here as an image anyway. 
#
# #Defining a function for finding MSE quickly
# MSE_m <- function(m){
#   set.seed(1337) #Sets the random seed for reproducible results
#   rf_wine = randomForest(quality~., #Predict quality using every variables
#                              data=wine_train_value_df, #Use training data
#                              mtry=m, #Use m predictors
#                              importance =TRUE)
#   #Getting the MSE
#   #First use the model to obtain predicted values
#   rf_predict_quality = predict(rf_wine, #Use the rf model
#                          newdata = wine_test_df) #Use the test df which has remaining data.
#   #return the MSE
#   return(mean((rf_predict_quality - wine_test_df$quality)^2)  )
# }
# 
# #Use a for loop to iterate over all possible values of predictors. 
# #I'm creating a plot here to visualize the data
# #Load in library ggplot2 cause base R plots suck.
# library(ggplot2)
# 
# #Create an empty list for the number of variables m chosen.
# m_list = c()
# #Create an empty list for MSE associated with m
# m_mse_list = c()
# for(i in 1:ncol(red_white_df)){ #Test out the number of variables.
#   m_list[i] <- i #Store the number of variable to the list.
#   m_mse_list[i] <- MSE_m(i) #Get the MSE of the associated m.
# }
# #create a dataframe using the lists as indices
# m_mse_df <- data.frame(x = m_list,y = m_mse_list) 
# 
# #Plot the df
# #plot this dataframe
# #put points in the plot
# #add in the x-axis
# ggplot(m_mse_df,aes(x,y)) + geom_point() + xlab('Number of m variables') + ylab('MSE obtained from RF model')
# 
# #From the plot we can see that the optimal number of m variables for the random forest
# # is 4.
# 

##########################################################################
###################### For four Random Forest Model ######################
########################################################################## 

#Now that we determined our optimal m is 4. Let's actually build the model.
rf_wine = randomForest(quality~., #Predict quality using every variables
                          data=wine_train_value_df, #Use train-value data
                          mtry=4, #Use 4 predictors
                          importance =TRUE)

#Use importance on the model to determine optimal number of variables.
importance(rf_wine)
#The most important predictors are
#Alcohol, volatile.acidity.

#Modified code from section 4
rf_wine_pred=predict(rf_wine,newdata=wine_test_df) #Get predictions of test set

#Plot y vs yhat for test data
rf_wine_pred_rmse = sqrt(mean((wine_test_df$quality-rf_wine_pred)^2) ) #Test set RMSE
cat('RMSE: ',rf_wine_pred_rmse,'\n')
par(mfrow = c(1,1)) #Plot window: 1 row, 1 column
plot(wine_test_df$quality,rf_wine_pred, #Scatterplot for quality of the fit
     xlab='Test Data quality',ylab='Random Forest prediction')
abline(0,1,col='red',lwd=2)

par(mfrow = c(1,1)) #Plot window: 1 row, 1 column
#Plot variable importance
varImpPlot(rf_wine)
#If we look at the plot, we can see the visual representation of what's good and bad.
#Basically Alcohol - Volatile Acidity - Free Sulphur dioxide - Sulphates - pH - residual.sugar

##########################################################################
###################### Boosted Model #####################################
########################################################################## 

#load in gbm library
library(gbm)

#build boost model using gbm function from gbm library.
boost_wine = gbm(quality~., #Predict quality using all the predictors.
                    data= wine_train_value_df, #Use the train-value dataframe
                    distribution= 'gaussian', #Use a Gaussian distribution 
                    # since it is a regression problem.
                    shrinkage = 0.2, #Use 0.2 for shrinkage
                    n.trees =100) #Use 100 trees 
#Use summary to get importance
summary(boost_wine)
#Boosted model gives us alcohol, volatile.acidity, free.sulfur.dioxide in that order.
yhat_boost = predict(boost_wine, newdata = wine_test_df)

mean((yhat_boost-wine_test_df$quality)^2)
#MSE for this boosted model is worse.
#I'll stop here for now and explore if we can improve the boosted model later.

