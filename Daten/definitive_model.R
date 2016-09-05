library(ggplot2) # für Grafik ROC-Kurve, zitieren
#library(MASS)
library(pROC) # für ROC-Kurve und AUC-Berechnung + Confidenzintervall, zitieren!

geon.all <- readRDS("tidy_data.rds")
geon.all$living_alone[is.na(geon.all$sozial)] <- NA

#Daten für erklärendes Modell nogo ohne iadl
geon.scales1.exp <- with(geon.all, data.frame(barthel_o = barthel,
                                          barthel = scale(barthel),
                                          mmst_o = mmst,
                                          mmst = scale(mmst),
                                          beschluss = beschluss_f))

geon.scales1.exp <- geon.scales1.exp[complete.cases(geon.scales1.exp),]

geon.scales1.exp$prediction_p <- numeric(nrow(geon.scales1.exp))

geon.scales1.exp$beschluss <- geon.scales1.exp$beschluss != "no_go"
geon.scales1.exp$beschluss <-
    factor(as.numeric(geon.scales1.exp$beschluss),
           levels = 0:1, labels=c("no_go", "go"))

#Daten für vorhersagendes Modell nogo mit iadl
geon.scales1.conf <- with(geon.all, data.frame(barthel_o = barthel,
                                          barthel = scale(barthel),
                                          mmst_o = mmst,
                                          mmst = scale(mmst),
                                          iadl = scale(iadl),
                                          beschluss = beschluss_f))

geon.scales1.conf <- geon.scales1.conf[complete.cases(geon.scales1.conf),]

geon.scales1.conf$prediction_p <- numeric(nrow(geon.scales1.conf))

geon.scales1.conf$beschluss <- geon.scales1.conf$beschluss != "no_go"
geon.scales1.conf$beschluss <-
    factor(as.numeric(geon.scales1.conf$beschluss),
           levels = 0:1, labels=c("no_go", "go"))

#Daten für erklärendes Modell slowgo, mit living_alone
geon.scales2.exp <- with(geon.all, data.frame(mmst = scale(mmst),
                                         tinetti = scale(tinetti),
                                         alter = scale(alter),
                                         mna = scale(mna),
                                         charlson = scale(charlson),
                                         living_alone = living_alone,
                                         beschluss = beschluss_f))

geon.scales2.exp <- geon.scales2.exp[complete.cases(geon.scales2.exp),]
geon.scales2.exp <- geon.scales2.exp[geon.scales2.exp$beschluss!="no_go",]
geon.scales2.exp$prediction_p <- numeric(nrow(geon.scales2.exp))
geon.scales2.exp$beschluss <-
    factor(as.numeric(geon.scales2.exp$beschluss)-2,
           levels = 0:1, labels=c("slow_go", "go_go"))

#Daten für vorhersagendes Modell slowgo, ohne living_alone
geon.scales2.conf <- with(geon.all, data.frame(mmst = scale(mmst),
                                          tinetti = scale(tinetti),
                                          alter = scale(alter),
                                          mna = scale(mna),
                                          charlson = scale(charlson),
                                          beschluss = beschluss_f))

geon.scales2.conf <- geon.scales2.conf[complete.cases(geon.scales2.conf),]
geon.scales2.conf <- geon.scales2.conf[geon.scales2.conf$beschluss!="no_go",]
geon.scales2.conf$prediction_p <- numeric(nrow(geon.scales2.conf))
geon.scales2.conf$beschluss <-
    factor(as.numeric(geon.scales2.conf$beschluss)-2,
           levels = 0:1, labels=c("slow_go", "go_go"))

#Erklärendes Modell:
#iadl wegen hoher Kolinearität mit barthel ausgeschlossen
g1 <- glm(beschluss=="no_go" ~ barthel + mmst,
          data=geon.scales1.exp, family="binomial")
summary(g1)

g2 <- glm(beschluss=="slow_go" ~ mmst + tinetti + alter + living_alone +
              charlson + mna,
          data=geon.scales2.exp, family="binomial")
summary(g2)

#Confirmatory no go

set.seed(1)
train <- sample(nrow(geon.scales1.conf), round(0.66*nrow(geon.scales1.conf)))
# 66 % im Trainingset

g3 <- glm(beschluss=="no_go" ~ barthel + mmst + iadl,
          data=geon.scales1.conf, family="binomial", subset = train)
summary(g3)

g3.roc <- roc(geon.scales1.conf$beschluss, geon.scales1.conf$prediction_p,
              smooth=T, auc=T, ci=T,plot=T)
g3.roc
g3.data <- data.frame(sens=g3.roc$sensitivities, spec=g3.roc$specificities)
textSize = 10
g3.p <- ggplot(aes(x=1-spec, y=sens), data=g3.data) +
    geom_line() +
    labs(x = "1 - Spezifität", y = "Sensitivität") +
    theme(axis.title.x = element_text(size=textSize),
          axis.title.y = element_text(size=textSize))
g3.p

# Confirmatory go

set.seed(1)
train <- sample(nrow(geon.scales2.conf), round(0.75*nrow(geon.scales2.conf))) # 75 % im Trainingset

g4 <- glm(beschluss=="slow_go" ~ mmst + tinetti + alter + mna + charlson,
          data=geon.scales2.conf, family="binomial", subset = train)
summary(g4)

geon.scales2.conf$prediction_p <-
    predict(g4, geon.scales2.conf, type = "response")
geon.scales2.conf$prediction <-
    factor(as.numeric(geon.scales2.conf$prediction_p < 0.5),
           levels = 0:1, labels=c("slow_go", "go_go"))

pred.tb <- 
    table(konferenz = geon.scales2.conf$beschluss[-train],
          modell = geon.scales2.conf$prediction[-train])
pred.tb
round(sum(diag(pred.tb))/sum(pred.tb),2)

g4.roc <- roc(geon.scales2.conf$beschluss, geon.scales2.conf$prediction_p,
    smooth=T, auc=T, ci=T,plot=T)

g4.data <- data.frame(sens=g4.roc$sensitivities, spec=g4.roc$specificities)
g4.p <- ggplot(aes(x=1-spec, y=sens), data=g4.data) +
    geom_line() +
    labs(x = "1 - Spezifität", y = "Sensitivität") +
    theme(axis.title.x = element_text(size=textSize),
          axis.title.y = element_text(size=textSize))
g4.p
