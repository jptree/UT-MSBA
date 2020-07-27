library(tree)



#######  SETUP  ############
# Make randomness....consistent.
rm(list = ls())
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



df_tree = red_white_df[, c(1, 12, 13)]

temp = tree(quality~ ., #Formula
           data=df_tree, #Data frame
           mindev=.0001) #The within-node deviance must be at least

#this times that of the root node for the node to be split
cat('First create a big tree size: \n')
print(length(unique(temp$where))) #Number of leaf nodes

#Then prune it down to one with 7 leaves
wine.tree=prune.tree(temp, #The tree model
                       best=7) #Only the seven best nodes
cat('Pruned tree size: \n')
print(length(unique(wine.tree$where))) #Number of new leaf nodes

par(mfrow=c(1,2)) #Plot window: 1 row, 2 columns
#Plot the tree
plot(wine.tree,
     type="uniform") #branches of uniform length
text(wine.tree,col="blue",label=c("yval"),cex=.8)
#Plotting the covariate space
partition.tree(wine.tree)

###########
library(gbm)

set = 1:nrow(red_white_df)
train_indicies = sample(set, size = floor(nrow(red_white_df) * .8))
train = red_white_df[train_indicies, ]
test = red_white_df[-train_indicies, ]

n_trees = 5000
finb = gbm(quality ~ .,
           data = train,
           distribution = 'gaussian',
           n.trees = n_trees,
           shrinkage = .2)
finbpred = predict(finb, newdata = test, n.trees = n_trees)

#Plot y vs yhat for test data and compute rmse on test.
finbrmse = sqrt(sum((test$quality-finbpred)^2)/nrow(test)) #Test RMSE
cat('finbrmse: ',finbrmse,'\n')
plot(test$quality,finbpred, #Scatterplot for the quality of fits
     xlab='test quality',ylab='boost pred')
abline(0,1,col='red',lwd=2)

#Plot variable importance
p=ncol(train)-1 #Get number of variables for later
vsum=summary(finb) #this object will have the variable importance info
row.names(vsum)=NULL #Drop varable names from rows.

print(vsum)

