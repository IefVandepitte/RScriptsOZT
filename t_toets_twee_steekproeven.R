control <-      c( 91, 87, 99,77,88, 91)
intervention <- c(101,110,103,93,99,104)

verschil_in_gemiddelden <- 0
alfa <- 0.05

t.test(x = control,
       y = intervention,
       alternative="less",
       mu = verschil_in_gemiddelden,
       paired = F,
       conf.level = 1-alfa)

# de teststatistiek mean(control)-mean(intervention) = -12.833 komt overeen met een t-waarde van -3.4456
# de teststatistiek df wordt berekend op basis van length(control) en length(intervention)
# p-waarde 0.003391 ligt duidelijk onder alfa (0.05) => H0 verwerpen
# er is dus minder dan 5% kans dat de twee populatis een gelijk gemiddelde kunnen hebben 