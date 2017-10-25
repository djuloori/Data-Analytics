#Load Required liraries
library('dplyr') 
library('ggplot2') 
library('ggthemes')
#Survivalrate v/s age interval 
train.data = read.csv('train.csv', stringsAsFactors = F)
test.data = read.csv('test.csv', stringsAsFactors = F)
test.data$Survived = NA
#bind train and test data to make it full data set
full = rbind(train.data,test.data)
#load age  to variable age 
age = full$Age
#Dataframe with all age columns and corresponding Survived values
dplot = data.frame(Age = age[1:891], Survived = train.data$Survived)
# ggplot + histogram with Survived vs Age 
ggplot(dplot, aes(Age,fill = factor(Survived))) + geom_histogram()