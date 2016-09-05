#Vorhersagen mit den Modellen aus modell.Rmd, interaktiv in predict-Zeilen die
#interessierenden Werte eingetragen

mb <- mean(geon.all$barthel, na.rm = T)
sb <- sd(geon.all$barthel, na.rm = T)
mm <- mean(geon.all$mmst, na.rm = T)
sm <- sd(geon.all$mmst, na.rm = T)

prediction1 <- data.frame(barthelO=seq(0,100,5),
                          barthel = (seq(0,100,5)-mb)/sb,
                          mmstO = 30,
                          mmst = (30-mm)/sm,
                          p = NA)

prediction1$p <- predict(g1, prediction1, type = "response")
qplot(barthelO, p, data = prediction1)

prediction2 <- data.frame(barthelO=100,
                          barthel = (100-mb)/sb,
                          mmstO = 0:30,
                          mmst = (0:30-mm)/sm,
                          p = NA)

prediction2$p <- predict(g1, prediction2, type = "response")

predict(g1, data.frame(barthel=(75-mb)/sb, mmst=(27-mm)/sm), type = "response")

mt <- mean(geon.all$tinetti, na.rm = T)
st <- sd(geon.all$tinetti, na.rm = T)
ma <- mean(geon.all$alter, na.rm = T)
sa <- sd(geon.all$alter, na.rm = T)
mc <- mean(geon.all$charlson, na.rm = T)
sc <- sd(geon.all$charlson, na.rm = T)
mmn <- mean(geon.all$mna, na.rm = T)
smn <- sd(geon.all$mna, na.rm = T)

prediction3 <- data.frame(tinetti=(0:28-mt)/st,
                          alter=(65-ma)/sa,
                          charlson=(0-mc)/sc,
                          living_alone=FALSE,
                          mmst = (30-mm)/sm,
                          mna = (28.5-mmn)/smn,
                          p = NA)

prediction3$p <- predict(g2, prediction3, type = "response")

predict(g2, data.frame(tinetti=(20-mt)/st, alter=(71-ma)/sa,
    charlson=(2-mc)/sc, living_alone=FALSE, mmst = (27-mm)/sm,
    mna = (24-mmn)/smn), type = "response")
