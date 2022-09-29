#Hello I'm gonna start this up. For my distribution analysis.

#Read in the csv file
#header = TRUE is here to specify the first row being variable names.
#Things to note about data set:
#There are 11 input or x variables.
#There is 1 y or output variable: quality. Quality is on a scale from 1 to 10.
redwine <- read.csv('wine_red.csv', header = TRUE)
whitewine <- read.csv('wine_white.csv', header = TRUE)

#There are noticeably more observations for white wine than red wine.
#White whine is probably better to do models such as K-nearest neighbors on.

#Attach is used to create objects for R to reference
attach(redwine)
attach(whitewine)

#Display summary statistics using summary() to get a general idea of the data.
summary(redwine)
summary(whitewine)

#Summary Statistics analysis:
#Red wine - all of the attributes have a very close mean to the median. 
#The same applies for the white whine dataset.
#This means that the wines are pretty similar (kinda expected since we're just looking at 1 type of alcoholic beverage)
#Even "worse" it's literally just Portuguese wine. No wonder they're all pretty similar!

#For the input variables, I don't want to go too much further in detail with them. I'm not too much of an expert on what makes wine wine.
#But I do think that if we decide on a linear regression model, we will have some multi-collinearity issues.
#Notably pH and alcohol are obviously related linearly. 
#The acids are most likely also related in this way. They also contribute to pH.
#I don't quite remember why multi-collinearity is bad but it'll make our model worse if we include those two.
#Further tests for that can come a bit later.

#Red wine quality analysis:
#Display summary statistics and standard deviation for our only output variable
#Reminder of notation: objectname$columnname
summary(redwine$quality)
sd(redwine$quality)
#Quality is supposed to be on a scale from 1 to 10 but there are no mins of 1 nor maxes of 10
#Instead we have a min of 3 and a max of 8
#This leads me to believe that quality is subjective and most likely comes from critics.
#Assuming that the scale is actually used in good faith from 1-10, I think the Portuguese wine is mostly mediocre.
#Assuming mediocre is around the 5-6 range. 
#However, it could be the case where wine critics are just harsh and scores from 5-6 are actually pretty good instead of mediocre.
#Regardless, most of the observations lie around the 5-6 range.
#This can be seen from the mean of 5.636 and the low sd of 0.807.



