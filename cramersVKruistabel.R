#library(foreign)
setwd(dir = "A:/schooleke/schooljaar4/semester2/onderzoekstechnieken-cursus/cursus/data")
resto <- read.spss("catering_hogeschool.sav",
                   to.data.frame = TRUE)

# Bereken een frequentietabel. Eerst de afhankelijke, vervolgens
# de onafhankelijke variabele.
observed <- table(resto$Keuze_basis,
                  resto$Geslacht)
# Voeg rij- en kolomtotalen toe aan de tabel
addmargins(observed)

row_sums <- rowSums(observed)              # rijtotalen
col_sums <- colSums(observed)              # kolomtotalen
n <- sum(observed)                         # totaal hele tabel
expected <- outer(row_sums, col_sums) / n  # verwachte waarden
addmargins(expected)                       # voeg totalen toe

expected - observed

diffs <- (expected - observed)^2 / expected
diffs

chi_squared <- sum(diffs)
chi_squared

#goodness of fit test
# H0 de steekproef is representabel voor de populatie (proporties van de klassen komen goed overeen)
# H1 de steekproef is niet representabel (veel verschil in de proporties)

alfa <- 0.05 #95% zekerheid
lengte <- ncol(observed)
lengte
g <- qchisq(p = 1 - alfa, df = lengte -1)

#bepaal de p-waarde
p <- 1 - pchisq(chi_squared, df = lengte - 1)
p

#n the case of the critical value g:
#  if ??2<g, accept the null hypothesis,
#if ??2>g, reject the null hypothesis
#In the case of the p-value:
#  if p>??, accept the null hypothesis,
#if p<??, reject the null hypothesis

# Critical value $g$
paste(ifelse(chi_squared < g, "Accept", "Reject"), "the null hypothesis")

# Plot the chi-squared density function
x <- seq(-15, 15, length = 100)
dist <- dchisq(x, df = lengte - 1)
plot (x, dist, type = 'l', xlab = '', ylab = '')

# The acceptance region (where H_0 is accepted)
i <- x <= g
polygon(c(x[i],    g,                     g),
        c(dist[i], dchisq(g, df = lengte - 1), 0),
        col = 'lightgreen')
text(x = 4, y = 0.05, 'Acceptance region (H0)')
text(x = g, y = 0.01, paste('g = ', round(g, digits=2)))

# The test statistic (chi squared)
abline(v = chi_squared, col = 'red')
text(x = chi_squared, y = 0.15, paste('chi^2 = ', round(chi_squared, digits=2)))


k <- min(nrow(observed), ncol(observed))
cramers_v <- sqrt(chi_squared / ((k - 1) * n))
cramers_v

library(vcd)
assocstats(observed)

V <- cramers_v

if (V == 0) {
  message("geen samenhang")
} else if (V > 0 && V < 0.1) {
  message("geen tot zwakke samenhang")
} else if (V == 0.1) {
  message("zwakke samenhang")
} else if (V > 0.1 && V < 0.25) {
  message("zwakke tot redelijk sterke samenhang")
} else if (V == 0.25) {
  message("redelijk sterke samenhang")
} else if (V > 0.25 && V < 0.5) {
  message("redelijk sterke tot sterke samenhang")
} else if (V == 0.75) {
  message("sterke samenhang")
} else if (V > 0.75 && V < 1) {
  message("sterke tot zeer sterke samenhang")
} else if (V == 1) {
  message("zeer sterke samenhang")
}

plot(t(observed), main ="mozaic")
barplot(observed, beside = T, main = "clustered bar chart",legend = T)

percentages <- prop.table(observed, 2)
barplot(percentages, horiz = T, main = "stacked percentage chart", legend = T)

