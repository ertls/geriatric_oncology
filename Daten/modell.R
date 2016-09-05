# Modell der Geriatrisch-onkologischen Konferenz, 30.9.2015

library(nnet) # für multinom
library(irr) # für kappa2

geon.all <- readRDS("tidy_data.rds")
geon.all$tinetti_raw <- geon.all$tinetti
geon.all$tinetti <- geon.all$tinetti_1 + geon.all$tinetti_2
geon.all$alter <- trunc(geon.all$alter / 365.25)

geon <- data.frame(mmst = geon.all$mmst, iadl = geon.all$iadl,
              barthel = geon.all$barthel, tug = geon.all$tug,
              tinetti = geon.all$tinetti, beschluss_f = geon.all$beschluss_f,
              personal_assessment_f = geon.all$personal_assessment_f)

set.seed(1)
train <- sample(nrow(geon), round(0.75 * nrow(geon))) # 75 % im Trainingset
test.fit <- glm(beschluss_f ~ mmst + iadl + barthel + tug + tinetti,
    #alter + barthel + iadl + mmst + tug + tinetti + mna + charlson,
                data = geon, family = binomial)
summary(test.fit)

geon <- geon[complete.cases(geon),]

test.fit <- multinom(beschluss_f ~ barthel + iadl + mmst + tug + tinetti,
                     data = geon, family = binomial, subset = train)

geon$prediction <- NA
geon$prediction <- predict(test.fit, geon, type = "class") #für multinom
#geon$prediction <- predict(test.fit, type = "response") # für glm

diff_konf <- table(personal = geon$personal_assessment_f,
                       konferenz = geon$beschluss_f)
kappa2(diff_konf)

pred <- table(prediction = geon$prediction[-train],
              beschluss = geon$beschluss_f[-train])
pred
sum(diag(pred)) / sum(pred)

