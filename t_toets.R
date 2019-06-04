# 1. Formuleer de hypothesen:
#  H0:??=3.3
#  H1:?? > 3.3
# 2. Kies significantieniveau, bv. ??=0.05

# We hebben een steekproef met
n <- 20      # steekproefgrootte
sm <- 3.483  # gemiddelde van de steekproef
ss <- 0.55   # standaardafwijking van de steekproef
a <- 0.05    # significantieniveau (gekozen door de onderzoeker)
m0 <- 3.3    # hypothetisch populatiegemiddelde (H0)

# 3. Bepaal de waarde van de toetsingsgrootheid, in dit geval steekproefgemiddelde = 3.483

p <- 1 - pt((sm - m0) / (ss/sqrt(n)), df = n-1)
p

# 4. Bepaal de overschrijdingskans p en verwerp H0 als p < ??
if(p < a) {
  print("H0 verwerpen")
} else {
  print("H0 niet verwerpen")
}

# 5. Bij de berekening van de kritieke grenswaarde gebruiken we de qt-functie met n???1 vrijheidsgraden.
g <- m0 + qt(1-a, df = n-1) * s / sqrt(n)
g

# Als het gevonden steekproefgemiddelde onder g ligt, kan je H0 niet verwerpen
if (sm < g) {
  print("H0 niet verwerpen")
} else {
  print("H0 verwerpen")
}

# grenzen van de plot (x-waarden)
x <- seq(m0-4*ss/sqrt(n), m0+4*ss/sqrt(n), length=200)
# y-waarden (volgen de Gauss-curve van de t-verdeling)
dist <- dt((x-m0)/(ss/sqrt(n)), df = n-1) * ss/sqrt(n)
plot (x, dist, type = 'l', xlab = '', ylab = '')

# Het aanvaardingsgebied plotten
# Waarden van x links van g
i <- x < g                    
# waarde dichtheidsfunctie voor g
dg <- dt((g-m0)/(ss/sqrt(n)),df=n-1) * ss/sqrt(n)
# Plot deze waarden op de grafiek
polygon(
  c(x[i],    g, g),
  c(dist[i], dg, 0),
  col = 'lightgreen')
text(m0, 0.02, 'aanvaardingsgebied (H0)')

# Trek een vertikale lijn bij het hypothetische populatiegemiddelde
text(m0, 0.01, m0)
abline(v=m0)

# Toon de kritieke grenswaarde
text(g+.025,.005,signif(g, digits=4))

# Toon het gevonden steekproefgemiddelde ahv rode vertikale lijn
abline(v=sm, col='red')
text(sm-.025, .04, sm, col = 'red')




#ttest easy way, alternative greater = rechtszijdig, conf.level = 1- alfa
x <- c(0.593,0.142,0.329,0.691,0.231,0.793,0.519,0.392,0.418)
mean(x)
t.test(x = x, alternative = "greater", mu = m0, conf.level = 1-a)