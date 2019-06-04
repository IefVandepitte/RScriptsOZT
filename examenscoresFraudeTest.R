setwd(dir = "A:/schooleke/schooljaar4/semester2/onderzoekstechnieken-cursus/oefeningen/data/hfst5_toetsing")
puntenlijst <- read.csv(file = "puntenlijst.csv", header = T, sep = ",", dec = ".")
A <- subset(x = puntenlijst, subset = puntenlijst$Groep == "A" & puntenlijst$Score != "NA" )
B <- subset(x = puntenlijst, subset = puntenlijst$Groep == "B" & puntenlijst$Score != "NA" )
C <- subset(x = puntenlijst, subset = puntenlijst$Groep == "C" & puntenlijst$Score != "NA" )
D <- subset(x = puntenlijst, subset = puntenlijst$Groep == "D" & puntenlijst$Score != "NA" )
E <- subset(x = puntenlijst, subset = puntenlijst$Groep == "E" & puntenlijst$Score != "NA" )
F <- subset(x = puntenlijst, subset = puntenlijst$Groep == "F" & puntenlijst$Score != "NA" )
G <- subset(x = puntenlijst, subset = puntenlijst$Groep == "G" & puntenlijst$Score != "NA" )
H <- subset(x = puntenlijst, subset = puntenlijst$Groep == "H" & puntenlijst$Score != "NA" )
summary(puntenlijst)


printspreiding_centrum <- function(x) {
  
  mean(x = x)
  message("gemiddelde: ",mean(x))
  sd(x = x)
  message("standaardafwijking: ",sd(x))
  summary(x)
  boxplot(x = x, names = T)
  
}
printspreiding_centrum(A$Score)

ptnGem <- aggregate(Score~Groep, data=puntenlijst, FUN = function(x) c(gemiddelde = mean(x)))
names <- c("A","B","C","D","E","F","G","H")
barplot(ptnGem$Score,names.arg=names, main = "staafdiagram per groep",xlab = "Groep",ylab = "Score")

boxplot(puntenlijst$Score~puntenlijst$Groep)

t.test(A$Score,C$Score,  alternative = "l", mu = 0)
