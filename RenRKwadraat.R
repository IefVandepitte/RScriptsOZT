# oefening 6.6

setwd(dir = "A:/schooleke/schooljaar4/semester2/onderzoekstechnieken-cursus/oefeningen/data/hfst6_2variabelen")
cats <- read.csv(file = "Cats.csv", sep = ",")

regressie <- subset(cats, select = c(Hwt,Bwt))

# gemiddeldes van x en y berekenen
meanx <- mean(regressie$Hwt)
meany <- mean(regressie$Bwt)

# kolommen toevoegen met tussenresultaten
regressie[,3] <- regressie$Hwt - meanx
colnames(regressie)[3] <- "x-mean(x)"
head(regressie)

regressie[,4] <- regressie$Bwt - meany
colnames(regressie)[4] <- "y-mean(y)"
head(regressie)

regressie[,5] <- regressie$`x-mean(x)` * regressie$`y-mean(y)`
colnames(regressie)[5] <- "(x-mean(x))*(y-mean(y))"
head(regressie)

regressie[,6] <- regressie$`x-mean(x)` ^ 2
colnames(regressie)[6] <- "(x-mean(x))^2"
head(regressie)

regressie[,7] <- regressie$`y-mean(y)` ^ 2
colnames(regressie)[7] <- "(y-mean(y))^2"
head(regressie)

# scatter plot plotten
plot(x = Hwt, y = Bwt)


B1 <- sum(regressie$`(x-mean(x))*(y-mean(y))`) / sum(regressie$`(x-mean(x))^2`)
B1
B0 <- meany - B1 * meanx
B0

abline(a = B0,  # snijpunt y-as
       b = B1,  # richtingscoëfficiënt
       col = 'red')


cov <- sum(regressie$`(x-mean(x))*(y-mean(y))`) / (nrow(regressie)-1)
cov

R <- cov / (sd(regressie$Hwt) * sd(regressie$Bwt))
R
V <- abs(R)
if (V < 0.3) {
  message("zeer zwak")
} else if (V > 0.3 && V < 0.5) {
  message("zwak")
} else if (V > 0.5 && V < 0.7) {
  message("matig")
} else if (V > 0.7 && V < 0.85) {
  message("sterk")
} else if (V > 0.85 && V < 0.95) {
  message("zeer sterk")
} else if (V > 0.95) {
  message("uitzonderlijk")
}
if (R >0) {
  message("stijgend")
} else if (R < 0) {
  message("dalend")
} else if (R == 0) {
  message("geen verband")
}
R_sq <- R ^ 2
R_sq

abline(h = meany, col = "green")
abline(v = meanx, col = "green")