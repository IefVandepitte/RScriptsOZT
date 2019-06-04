setwd(dir = "A:/schooleke/schooljaar4/semester2/onderzoekstechnieken-cursus/cursus/data")
gewichtstoename <- read.csv("santa.txt",sep = "")

#x = eiwitgehalte
#y = gewichtstoename

attach(gewichtstoename)
plot(x,y,
     main = "gewichtstoename",
     xlab = "eiwitgehalte(%)",
     ylab = "gewichtstoename(g)")

#bereken regressie lineaire model
regr <- lm( y ~ x)
abline(regr, col = "red")