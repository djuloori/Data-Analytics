
#Load Required liraries
library('dplyr') 
library('ggplot2') 
library('ggthemes')
#barchart for Survivalrate v/s age interval
cuts = cut(d$Age,hist(d$Age,10,plot = F)$breaks)
rate = tapply(d$Survived,cuts,mean)
d2 = data.frame(age = names(rate),rate)
x = cumsum(rnorm(50))
cols <- c("red")[(x > 0) + 1]
barplot(d2$rate, xlab = "age",ylab = "survival rate", col = cols)

# ggplot with Variable Survived vs sex
-------------------------------------------------------------------------------------
ggplot(train, aes(Sex,fill = factor(Survived))) +
    geom_histogram(stat = "count")
--------------------------------------------------------------------------------
