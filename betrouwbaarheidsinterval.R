# Stap 1
m <- 5.2      # steekproefgemiddelde
s <- 1.5      # standaardafwijking van de populatie
n <- 100      # steekproefgrootte
alpha <- 0.05 # 1 - alpha is het zekerheidsniveau
# Stap 2
# z score voor het zekerheidsniveau van een standaardnormale verdeling
z <- qnorm(p = 1-alpha/2) # waarom delen door 2?
z

# Stap 3: het betrouwbaarheidsinterval
low  <- m - z * s / sqrt(n)
high <- m + z * s / sqrt(n)
c(low, high)