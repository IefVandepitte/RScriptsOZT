# Genereer 10000 willekeurige normaal verdeelde getallen:
n <- 10000
m <- 183
s <- 36
observations <- rnorm(n, m, s)

# Teken een histogram van de relatieve ipv de absolute
# frequenties.
# Speel met de waarde van "breaks" om een "fijner" of "grover"
# histogram te bekomen.
hist(observations,probability = T, breaks = 50)

x <- seq(from = min(observations),
         to = max(observations), 
         length.out = n)
# Bereken de dichtheidsfunctie
y <- dnorm(x, m, s)

# Bereken y-waarden voor de dichtheidsfunctie
y <- dnorm(x, m, s)

# Voeg deze functie toe aan het histogram
lines(x, y, col = 'blue')


1- pnorm( 55,mean = 43.1, sd = 6.6)

