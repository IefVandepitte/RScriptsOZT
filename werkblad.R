library(MASS)


#De (vermoedelijk) onafhankelijke variabele komt eerst
ExerSmoke <- table(Aids2$sex, Aids2$T.categ)


# Functions that checks whether the specified contingency table x
# satisfies Cochran's rule.
cochrans_rule <- function(x) {
  test <- chisq.test(x)
  EXPECTED = test$expected
  
  # Rule 1. None of the expected values should be less than 1
  message("None of the expected values should be less than 1")
  EXP_LT_1 = table(EXPECTED < 1)
  NUM_LT_1 = as.numeric(EXP_LT_1['TRUE'])
  if(is.na(NUM_LT_1)) {
    NUM_LT_1 = 0
  }
  SAT_LT_1 = NUM_LT_1 == 0
  
  # Rule 2. At most 20% of expected values should be less than 5
  message("At most 20% of expected values should be less than 5")
  EXP_LT_5 = table(EXPECTED < 5)
  PCT_LT_5 = as.numeric(EXP_LT_5['TRUE']) / sum(EXP_LT_5)
  if(is.na(PCT_LT_5)) {
    PCT_LT_5 = 0
  }
  SAT_LT_5 = PCT_LT_5 < .2
  
  # Return final result and intermediate steps
  structure(list(
    satisfied = SAT_LT_1 & SAT_LT_5,
    
    expected = EXPECTED,
    
    expected.lt.1 = EXP_LT_1,
    satisfied.lt.1 = SAT_LT_1,
    
    expected.lt.5 = EXP_LT_5,
    satisfied.lt.5 = SAT_LT_5
  ))
}

cochrans_rule(ExerSmoke)
addmargins(ExerSmoke)
prop <- prop.table(ExerSmoke, 2)
barplot(ExerSmoke, beside = T, legend = T)

mosaicplot(ExerSmoke)
barplot(prop, horiz = T, legend =T)

library(MASS)
# View(survey) # Toont de "survey dataset"
# ?survey # help-pagina voor deze dataset met uitleg over de inhoud
# (a) denk eerst na welke uitkomst je precies verwacht voor de de opgegeven combinatie van variabelen
# (b) stel een frequentietabel op voor de twee variabelen. De (vermoedelijke) onafhankelijke komt eerst
# (c) plot een grafiek van de data, bv geclusterde staafgrafiek, gestapelde staafgrafiek van relatieve frequenties
#    of een mozaiekgrafiek (eenvoudig met plot(table(data$col1, data$col2))), 
# (d) als je de grafiek bekijkt, verwacht je dan eerder hoge of lage waarde voor de chi-kwadraat statistiek? waarom?
# (e) bereken X^2 statistiek en kritieke grenswaarde g (voor significantienivea alfa = 0.05)
# (f) bereken de p waarde
# (g) moeten we de nulhypothese aanvaarden of verwerpen? wat betekent dat concreet voor de relatie tusssen de twee variabelen
#    
#    1 Exer (sporten) en Smoke (roken)
#    2 W.Hand (hand waarmee je schrijft) en Fold (de hand die bovenaan komt als je de armen kruist)
#    3 Sex (gender) en Smoke
#    4 Sex en W.Hand

# 1A ik vermoed dat er een verband is sporters zullen wel minder roken
# 1b
# ExerSmoke <- table(survey$Exer, survey$Smoke, dnn = c("Exer", "Smoke"))
ExerSmoke
addmargins(ExerSmoke)
ExerSmokeProp <- prop.table(ExerSmoke,2)
ExerSmokeProp
# 1c
# plot: mozaic plot
plot(t(ExerSmoke))
# plot(table(survey$Exer, survey$Smoke))

# clustered bar chart
barplot(ExerSmoke, beside = TRUE, legend = T)

# stacked percentage chart
barplot(prop, legend = T)
# 1d ik vermoed geen verband rokers en niet rokers ongeveer evenredig verdeeld
# 1e 
# rechtstreeks berekenen chi-kwadraat
summ <- summary(ExerSmoke)
summ
chi_sq <- summ$statistic
chi_sq

df <- ((ncol(ExerSmoke) -1) * (nrow(ExerSmoke) -1))
df
alfa <- 0.05
# https://www.itl.nist.gov/div898/handbook/eda/section3/eda3674.htm
g <- 12.6
g <- qchisq(p = 1 - alfa, df = df )

H0 <- "in de populatie is er geen samenhang tussen de onafhankelijke en afhankelijke variabele"
H1 <- "er bestaat wel samenhang tussen de variabelen in de populatie"


toets <- chisq.test(ExerSmoke)

p <- toets$p.value

library("gplots")
library("graphics")
library('visualize')
balloonplot(t(ExerSmoke), main ="Exer Smoke", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot(ExerSmoke, shade = TRUE, las=2,
           main = "Relatie sport / roken")

visualize.chisq(stat = toets$statistic, df = df, section = "upper")

message("p: " ,p, ", alfa: ", alfa)
if(p > alfa){ message(H0)
} else {
  message (H1)
}

message("chi-kwadraat: ", chi_sq, ", g: ", g)
if (chi_sq < g) {
  message(H0)
} else {
  message (H1)
}

gestandaardiseerde_residueen <- toets$stdres
gestandaardiseerde_residueen
message("waarden groter dan 2 en waarden kleiner dan -2 zijn extreem")