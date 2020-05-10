#Dot Plot

# Adapted from scripts written RAWIII (https://github.com/raw937)

library("ggplot2")
library("RColorBrewer")
library("reshape")
library("reshape2")

Nilemah_Data <-read.csv("demo.csv", check.names = FALSE, header=TRUE)

Melted_Nilemah_Data<- melt(Nilemah_Data)

M1 <- subset(Melted_Nilemah_Data, value > 10) 

#Plot Dot Plot

ggplot(M1, aes(x=variable, y=Taxa, color=variable)) + geom_point(aes(size=(value))) + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
