m <-  2.5
s <-  1.5

#rechtergebied
y <- 4
#linkergebied
x <- 0.5



# interval van de plot (x-waarden)
x_interval <- seq(m - 4 * s, m + 4 * s, length=200)

# punten op de Gauss-curve
norm_dist <- dnorm(x_interval, m, s)
plot(x_interval, norm_dist,
     type = 'l', xlab = '', ylab = '')

# Het gebied links van x inkleuren
i <- x_interval <= y
polygon(
  c(x_interval[i], y,  y),
  c(norm_dist[i],  dnorm(y, m, s), 0),
  col = 'lightgreen')
text(y, .01, y)

# Toon het gemiddelde ahv een rode vertikale lijn
abline(v = m , col='red')
text(m, .01, m)

# bereken de p-waarde voor y

 y  <- pnorm(y, m, s)
 message("pwaarde voor y: ", y)

# Het gebied links van x inkleuren
i <- x_interval <= x
polygon(
  c(x_interval[i], x,  x),
  c(norm_dist[i],  dnorm(x, m, s), 0),
  col = 'white')
text(y, .01, y)

#bereken de p-waarde voor x

x <- pnorm(x,m,s)
message("pwaarde voor x: ", x)

#bereken oppervlakte van y-x == groene gebied
message("y-x= ", y-x)

