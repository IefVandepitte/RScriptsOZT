resultaten <- c(11.5,16.5,11,17.3,10.8,5.6,13.1,11.5,14.2,12.9,8.7,9.2,15,14.4,10,10.3,18.3,12.9,14.2,8.7)

summary(resultaten)

m <- mean(resultaten) # steekproefgemiddelde

s <- sd(resultaten)

n <- length(resultaten) # steekproefgrootte

alpha <- 0.08 # 1 - alpha is het zekerheidsniveau
 
sigma <- 2.45 # standaardafwijking van de populatie

# z score voor het zekerheidsniveau van een t verdeling
t <- qt(p = alpha/2 ,df = n-1)
t

# Stap 3: het betrouwbaarheidsinterval
low  <- m - t * s / sqrt(n)
high <- m + t * s / sqrt(n)
c(low, high)