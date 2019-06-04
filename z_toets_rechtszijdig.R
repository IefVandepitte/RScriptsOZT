#1. Formuleer de hypothesen:
#  H0:??=3.3
#  H1:??>3.3

#2.  Kies significantieniveau, bv.  ??=0.05


# We hebben een steekproef met
n <- 30      # steekproefgrootte
sm <- 3.483  # steekproefgemiddelde
s <- 0.55    # standaardafwijking (verondersteld gekend)
a <- 0.05    # significantieniveau (gekozen door de onderzoeker)
m0 <- 3.3    # hypothetisch populatiegemiddelde (H0)

#3. Bepaal de waarde van de toetsingsgrootheid, in dit geval steekproefgemiddelde =3.483

#A overschrijdingskans

#4. Bepaal de overschrijdingskans p en verwerp H0 als  p < ??

p <- 1 - pnorm(sm, m0, s/sqrt(n))
p

if(p < a) {
  print("H0 verwerpen")
} else {
  print("H0 niet verwerpen")
}

#B kritieke gebied
g <- m0 + qnorm(1-a) * s / sqrt(n)
g

# Als het gevonden steekproefgemiddelde onder g ligt, kan je H0 niet verwerpen
if (sm < g) {
  print("H0 niet verwerpen")
} else {
  print("H0 verwerpen")
}

#plot 
# grenzen van de plot (x-waarden)
x <- seq(m0-4*s/sqrt(n), m0+4*s/sqrt(n), length=200)
# y-waarden (volgen de Gauss-curve)
dist <- dnorm(x, m0, s/sqrt(n))
plot (x, dist, type = 'l', xlab = '', ylab = '')

# Toon het gevonden steekproefgemiddelde ahv rode vertikale lijn
abline(v=sm, col='red')
text(sm, 2, sm)

# Het aanvaardingsgebied plotten
i <- x <= g                    # Waarden van x links van g
polygon(                       # Plot deze waarden op de grafiek
  c(x[i],    g,                       g),
  c(dist[i], dnorm(g, m0, s/sqrt(n)), 0),
  col = 'lightgreen')
text(g,.5,signif(g, digits=4)) # Toon grenswaarde

text(m0, 0.1, m0)              # Hypothetisch populatiegemiddelde
abline(v=m0)                   # Trek daar een vertikale lijn

text(m0, 1.5, 'aanvaardingsgebied (H0)')