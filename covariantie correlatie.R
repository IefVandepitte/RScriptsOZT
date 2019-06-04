setwd(dir = "A:/schooleke/schooljaar4/semester2/onderzoekstechnieken-cursus/cursus/data")
families <- read.csv("families.txt", sep ="")

mx <- mean(families$x)
my <- mean(families$y)

plot(families$x,families$y)
abline(h = my, col = 'red')
abline(v = mx, col = 'red')

# Covariantie manueel berekend
covar <- sum((families$x - mx) * (families$y - my)) / (length(families$x) - 1)
covar

# R-functie
cov(families$x, families$y)

#correlantie
# Berekenen vanuit covariantie
covar / (sd(families$x) * sd(families$y))

# Uitgewerkte formule
sum((families$x - mx) * (families$y - my)) / 
  sqrt(sum((families$x- mx)^2 * sum((families$y - my)^2)))

# R-functie
cor(families$x, families$y)
 correlatiecoefficient <- cor(families$x, families$y)
 V <- abs(correlatiecoefficient)
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
 if (correlatiecoefficient >0) {
   message("stijgend")
 } else if (correlatiecoefficient < 0) {
  message("dalend")
 } else if (correlatiecoefficient == 0) {
   message("geen verband")
 }

regression <- lm( families$y ~ families$x)
abline(regression, col = "green")