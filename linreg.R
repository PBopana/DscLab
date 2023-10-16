install.packages("caTools")

library(caTools)
library(ggplot2)

data<-data.frame(
  Years_Exp = c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7),
  Salary = c(39343.00, 46205.00, 37731.00, 43525.00,
             39891.00, 56642.00, 60150.00, 54445.00, 64445.00, 57189.00)
)

# 1.scatterplot
plot(data$Years_Exp, data$Salary,
     xlab="Years_Exp",
     ylab="Salary",
     main="Scatter Plot",
     pch=19)

# 2.split
samples<-sample.split(data$Salary, SplitRatio = 0.7)
training<-subset(data, samples=="TRUE")
testing<-subset(data, samples=="FALSE")

# 3.fit model and find coef
linreg=lm(formula=Salary~Years_Exp,
          data=training)


coef(linreg)


# 4. y_pred
y_pred=predict(linreg, newdata=testing)


# 5. ggplot

ggplot()+
  geom_point(aes(x=training$Years_Exp,y=training$Salary),
             color='red')+
  geom_line(aes(x=training$Years_Exp,y=predict(linreg, training)),
            color='blue')+
  ggtitle("yoe vs sal-training")+
  xlab('Years of experience') +
  ylab('Salary')



ggplot()+
  geom_point(aes(x=testing$Years_Exp,y=testing$Salary),
             color='red')+
  geom_line(aes(x=training$Years_Exp,y=predict(linreg, training)),
            color='blue')+
  ggtitle("yoe vs sal-testing")+
  xlab('Years of experience') +
  ylab('Salary')
