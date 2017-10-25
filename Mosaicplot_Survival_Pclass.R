#Load required libraries to draw plots
library('dplyr') 
library('ggplot2') 
library('ggthemes')
#Reading the Data
train.data = read.csv("train.csv")
test.data = read.csv("test.csv")
#Plotting the survival rate of the passengers by class
train.data$Survived = factor(train.data$Survived, levels=c(1,0))
#Classify survived data to Survived and Died
levels(train.data$Survived) = c("Survived", "Died")
#Convert data type of a variable to a factor/categorical value
train.data$Pclass = as.factor(train.data$Pclass)
#Classify levles to first, second and 3rd Class
levels(train.data$Pclass) = c("1st Class", "2nd Class","3rd Class")
#Set plot width and height
png("survivalratebyclass.png", width=800, height=600)
#mosaicplot with Pclass(First, Second, Third Vs Survived)
mosaicplot(train.data$Pclass ~ train.data$Survived, main="Passenger Survival by Class",color=c("#008000", "#FF0000"), shade=FALSE,  xlab="", ylab="",off=c(0), cex.axis=1.4)
#To return output to terminal
dev.off()
