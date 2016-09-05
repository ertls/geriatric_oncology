library(ggplot2)

geon.all <- readRDS("geon.rds")

geon.scales <- with(geon.all, data.frame(barthel = scale(barthel),
                                         mmst = scale(mmst),
                                         tinetti = scale(tinetti),
                                         alter = scale(alter),
                                         living_alone = living_alone,
                                         mna = scale(mna),
                                         charlson = scale(charlson),
                                         iadl = scale(iadl),
                                         beschluss = beschluss_f))

geon.scales <- with(geon.all, data.frame(barthel = scale(barthel),
                                         mmst = scale(mmst),
                                         charlson = scale(charlson),
                                         beschluss = beschluss_f))

geon.scales$beschluss <- ordered(geon.scales$beschluss)

geon.scales <- geon.scales[complete.cases(geon.scales),]

#barthel:
g1 <- glm(beschluss=="no_go" ~ poly(barthel,3),
          data=geon.scales[c(-234,-304,-166,-145),], family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(barthel,1),
          data=geon.scales[c(-234,-243),], family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(barthel,1),
          data=geon.scales[c(-234,-243),], family="binomial")

#IADL
g1 <- glm(beschluss=="no_go" ~ poly(iadl,1),
          data=geon.scales[c(-276,-188),], family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(iadl,1),
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(iadl,1),
          data=geon.scales, family="binomial")

#Barthel und IADL
g1 <- glm(beschluss=="no_go" ~ poly(barthel,1) + poly(iadl,1),
          data=geon.scales, family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(barthel,2) + poly(iadl,2),
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(barthel,2) + poly(iadl,2),
          data=geon.scales, family="binomial")

which(rownames(geon.scales)=="205")

#MMST
g1 <- glm(beschluss=="no_go" ~ poly(mmst,1),
          data=geon.scales[c(-189,-68,-332,-96,-48),], family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(mmst,1),
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(mmst,1),
          data=geon.scales, family="binomial")

#DemTect
g1 <- glm(beschluss=="no_go" ~ poly(demtect,1),
          data=geon.scales, family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(demtect,1),
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(demtect,1),
          data=geon.scales, family="binomial")

#Tinetti
g1 <- glm(beschluss=="no_go" ~ poly(tinetti,1),
          data=geon.scales[c(-288),], family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(tinetti,1),
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(tinetti,1),
          data=geon.scales, family="binomial")

#Tug
g1 <- glm(beschluss=="no_go" ~ poly(tug,3),
          data=geon.scales[c(-26,-194),], family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(tug,1),
          data=geon.scales[c(-26,-180,-197),], family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(tug,3),
          data=geon.scales, family="binomial")

#Tug und Tinetti
g1 <- glm(beschluss=="no_go" ~ poly(tug,1) + poly(tinetti,1),
          data=geon.scales, family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(tug,1) + poly(tinetti,1),
          data=geon.scales[c(-26,-180,-197),], family="binomial")
g3 <- glm(beschluss<="slow_go" ~ poly(tug,1) + poly(tinetti,1),
          data=geon.scales[c(-26,-180,-197),], family="binomial")

#MNA
g1 <- glm(beschluss=="no_go" ~ poly(mna,1),
          data=geon.scales, family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(mna,1),
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(mna,1),
          data=geon.scales, family="binomial")

#BMI
g1 <- glm(beschluss=="no_go" ~ poly(bmi,2),
          data=geon.scales, family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(bmi,1),
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(bmi,1),
          data=geon.scales, family="binomial")

#BMI + MNA
g1 <- glm(beschluss=="no_go" ~ bmi + mna,
          data=geon.scales, family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(bmi,1) + poly(mna,1),
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(bmi,1) + poly(mna,1),
          data=geon.scales, family="binomial")

#Charlson
g1 <- glm(beschluss=="no_go" ~ poly(charlson,1),
          data=geon.scales, family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(charlson,1),
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(charlson,1),
          data=geon.scales, family="binomial")

#Sozial
g1 <- glm(beschluss=="no_go" ~ living_alone,
          data=geon.scales, family="binomial")
g2 <- glm(beschluss<="slow_go" ~ living_alone,
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(bmi,1),
          data=geon.scales, family="binomial")

#DIA
g1 <- glm(beschluss=="no_go" ~ poly(dia,1),
          data=geon.scales, family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(dia,1),
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(dia,1),
          data=geon.scales, family="binomial")

#GDS
g1 <- glm(beschluss=="no_go" ~ poly(gds,1),
          data=geon.scales, family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(gds,2),
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(gds,1),
          data=geon.scales, family="binomial")

#Alter
g1 <- glm(beschluss=="no_go" ~ poly(alter,1),
          data=geon.scales, family="binomial")
g2 <- glm(beschluss<="slow_go" ~ poly(alter,2),
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(alter,1),
          data=geon.scales, family="binomial")

#Kombinationen

g1 <- glm(beschluss=="no_go" ~ barthel + mmst,
          data=geon.scales, family="binomial")
g2 <- glm(beschluss<="slow_go" ~ mmst + tinetti + alter +
              living_alone + mna + charlson,
          data=geon.scales, family="binomial")
g3 <- glm(beschluss=="go_go" ~ poly(alter,1),
          data=geon.scales, family="binomial")

summary(g1)
summary(g2)
summary(g3)

mmst = seq(0, 30, by=1)
#tinetti = seq(0, 28, by=1)

geon.p <- data.frame(mmst = mmst,
                     beschluss = NA)

geon.p$barthelO <- geon.p$barthel
#geon.p$tinettiO <- geon.p$tinetti
geon.p$mmst <- (geon.p$mmst - mean(geon.all$mmst, na.rm=T))/
                       sd(geon.all$mmst, na.rm=T)
#geon.p$tinetti <- (geon.p$tinetti - mean(geon.all$tinetti, na.rm=T))/
#    sd(geon.all$tinetti, na.rm=T)

geon.p$beschluss.g1 <- predict(g1, geon.p, type="response")
geon.p$beschluss.g2 <- predict(g2, geon.p, type="response")
geon.p$beschluss.g3 <- predict(g3, geon.p, type="response")

p <- ggplot(aes(x=mmst), data = geon.p) +
    geom_point(aes(y=beschluss.g1, colour = "No go")) +
    geom_point(aes(y=beschluss.g2, colour = "Slow go")) +
    geom_point(aes(y=beschluss.g3, colour = "Go go")) #+
p

p <- ggplot(data=geon.scales[c(-332,-96,-48),]) +
    stat_sum(aes(mmst, as.numeric(beschluss=="no_go"), size = ..n..))
p

table(konferenz = geon$beschluss,
      modell = predict(g1, geon, type = "response")>0.5)
