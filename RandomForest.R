#install packages
install.packages("caTools")
install.packages("randomForest")

#load the packages
library(caTools)
library(randomForest)

#Load the data and get info about it(structure)
data(iris)

str(iris)

#create samples, train and test set
samples<-sample.split(iris, SplitRatio = 0.7)
samples

training<-subset(iris, samples=="TRUE")
testing<-subset(iris, samples=="FALSE")

#fit the model
set.seed(120)
rf_classifier = randomForest(x=training[-5],
                             y=training$Species,
                             ntrees=500)
rf_classifier


#predictions on the test set
y_pred = predict(rf_classifier, newdata=testing[-5])

#plot confusion matrix btw testing and the prediction made using the model
cf_matrix=table(testing[,5],y_pred)
cf_matrix

plot(rf_classifier) #plots error rates

importance(rf_classifier) #shows important feature

varImpPlot(rf_classifier) #plots importance