MyData <- read.csv(file="compos_familiar_2016.csv", header=TRUE, sep=",")
library(networkD3)
#install.packages("networkD3")

#Filtrado de los datos por la familia especificada
MyData_filtered = MyData[MyData$LLAVEHOG == 50040971,]

fathers = MyData_filtered[,c("P6081S1","ORDEN","P6020")]
colnames(fathers) <- c("Parent", "Person","Genre")
mothers = MyData_filtered[,c("P6083S1","ORDEN","P6020")]
colnames(mothers) <- c("Parent","Person","Genre")

parents = rbind(fathers,mothers)

people = fathers[,c("Person","Genre")]
people$Genre = people$Genre - 1

parentChild = parents[!is.na(parents$Parent),c("Parent","Person")]
parentChild$Parent = parentChild$Parent - 1
parentChild$Person = parentChild$Person - 1
parentChild$Value = 4

forceNetwork(Links = parentChild, Nodes = people, Source = "Parent", Target = "Person", Value = "Value", NodeID = "Person", Group = "Genre", arrows = TRUE, zoom = TRUE, fontSize = 12, opacity = 100)

#install.packages("forceNetwork")



