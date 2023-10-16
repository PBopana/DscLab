#install packages and load them
install.packages("caTools")

library(caTools)
library(ggplot2)

#create own dataset
data<-data.frame(
  yoe=c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7),
  sal=c(39343.00, 46205.00, 37731.00, 43525.00,39891.00, 56642.00, 60150.00, 54445.00, 64445.00, 57189.00)
)

str(data)

#scatter plot
plot(data$yoe, data$sal,
     xlab="years of experience",
     ylab="salary",
     main="scatter plot of years of experience vs salary",
     pch=19
     )

#sampling and splitting
samples<-sample.split(data$sal, SplitRatio = 0.7)
training<-subset(data, samples=="TRUE")
testing<-subset(data, samples=="FALSE")

#fit the model
lin_reg=lm(formula=sal~yoe, data=training)
coef(lin_reg)

y_pred=predict(lin_reg, newdata=testing)

#plot for training and testing
ggplot() +
  geom_point(aes(x=training$yoe, y=training$sal),
             colour='red')+
  geom_line(aes(x=training$yoe, y=predict(lin_reg, newdata=training)),
                colour='blue')+
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')
            
            
ggplot() +
  geom_point(aes(x=testing$yoe, y=testing$sal)
             ,colour='red')+
  geom_line(aes(x=training$yoe, y=predict(lin_reg, newdata=training)),
                colour='blue')+
  ggtitle('Salary vs Experience (Testing set)') +
  xlab('Years of experience') +
  ylab('Salary')


