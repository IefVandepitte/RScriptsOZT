# Bron: https://www.statmethods.net/advgraphs/probability.html
x <- seq(-4, 4, length=100) # x-waarden
std_norm_dist <- dnorm(x)   # standaardnormaalverdeling, ter vgl         
degf <- c(1, 3, 8, 30)      # te plotten vrijheidsgraden

# afwerking van de grafiek (kleur, legende)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normaal")

# plot van de standaardnormaalverdeling
plot(x, std_norm_dist,
     type="l", lty=2,
     xlab="x-waarde", ylab="dichtheid",
     main="Vergelijking van Student-t verdelingen")

# plot van de vier Student-t verdelingen
for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Verdelingen",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors,
       cex = .5)