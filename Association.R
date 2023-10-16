#install packages
install.packages("arules")
install.packages("arulesViz")

#load packages
library(arules)
library(arulesViz)

#load data and get structure
dataset<-read.transactions("C:/Users/gbopa/Downloads/Market_Basket_Optimisation.csv",
                           sep=",", rm.duplicates = TRUE)

str(dataset)

#fit the model
set.seed(220)
rules=apriori(data=dataset,
              parameter = list(supp = 0.004, conf = 0.2))

itemFrequencyPlot(dataset, topN=10)

inspect(sort(rules, by='lift')[1:10])

plot(rules, method="graph", 
     measure="confidence", shading="lift")
