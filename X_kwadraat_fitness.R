types      <- c("mutant", "human", "alien", "god", "demon")
observed   <- c(   127,      75,      98,     27,     73)
expected_p <- c(   .35,     .17,     .23,    .08,    .17)

# Formulate the hypotheses:

#   H0: the sample is representative for the population 
#       (i.e. the proportions of each class in the sample closely matches those of the population)
#   H1: the sample is not representative for the population 
#       (i.e. the proportions diverge significantly)
# Determine a significance level, e.g. ??=0.05 and the sample size


alpha <- 0.05
n <- sum(observed)
expected <- n * expected_p
expected

# bereken de test statistiek X kwadraat
chisq <- sum((observed - expected)^2 / expected)
chisq

# Determine the p-value or the critical value g.
# Remark that in practice, you only need to calculate one of the two. Both methods are equivalent.
# 
# In a ??2-test, the critical value is a number g with property P(??2>g)=??
# (where ?? is our chosen significance level). Left of g is the acceptance region,
# right of g the critical region (see the plot below). This number can be calculated with:

l <- length(types)
g <- qchisq(p = 1 - alpha, df = l - 1)
g

p <- 1 - pchisq(chisq, df = l - 1)
p

# Critical value $g$
paste(ifelse(chisq < g, "Accept", "Reject"), "the null hypothesis")

# Probability value $p$
paste(ifelse(p > alpha, "Accept", "Reject"), "the null hypothesis")

# Plot the chi-squared density function
x <- seq(0, 15, length = 100)
dist <- dchisq(x, df = l - 1)
plot (x, dist, type = 'l', xlab = '', ylab = '')

# The acceptance region (where H_0 is accepted)
i <- x <= g
polygon(c(x[i],    g,                     g),
        c(dist[i], dchisq(g, df = l - 1), 0),
        col = 'lightgreen')
text(x = 4, y = 0.05, 'Acceptance region (H0)')
text(x = g, y = 0.01, paste('g = ', round(g, digits=2)))

# The test statistic (chi squared)
abline(v = chisq, col = 'red')
text(x = chisq, y = 0.15, paste('chi^2 = ', round(chisq, digits=2)))





# library('visualize')
# toets <- chisq.test(x= observed)
# visualize.chisq(stat = toets$statistic, df = l-1, section = "upper")