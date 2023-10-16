install.packages("dplyr")
install.packages("caTools")
install.packages("ROCR")

library(dplyr)
library(ROCR)
library(caTools)

data(mtcars)

summary(mtcars)

samples<-sample.split(mtcars, SplitRatio=0.8)
training<-subset(mtcars, samples=="TRUE")
testing<-subset(mtcars, samples=="FALSE")

logreg=glm(vs~wt+disp,
           data=training, family="binomial")

logreg

pred=predict(logreg,newdata=testing, type="response")
pred <- ifelse(pred >0.5, 1, 0)


cf=table(testing$vs, pred)
cf

err=mean(pred!=testing$vs)
print(paste('Accuracy =', 1 - err))

rocpred=prediction(pred, testing$vs)
rocper=performance(rocpred, measure="tpr", x.measure = "fpr")

auc=performance(rocpred, measure="auc")
auc <- auc@y.values[[1]]
auc


plot(rocper)
plot(rocper, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1),
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)