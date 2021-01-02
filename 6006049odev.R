#Polinom Regresyonu

#install.packages("readxl")
#install.packages('ggplot2')
library(ISLR)
library(dplyr)
library('ggplot2')
library("readxl")

#veriyi oku
veri <- read_excel('banking_loanapproval.xlsx', sheet="bankcustomer")

#nümerik sütunları al
veri_kull <- veri[2:5]
View(veri_kull)
veri_kull$LoanAmountinK <- veri_kull$LoanAmountinK * 1000
View(veri_kull)


resInc <- cor.test(veri_kull$LoanAmountinK, veri_kull$Income, 
                method = "pearson")
resInc

resCrd <- cor.test(veri_kull$LoanAmountinK, veri_kull$CreditCards, 
                method = "pearson")
resCrd

resAcc <- cor.test(veri_kull$LoanAmountinK, veri_kull$LoanAccounts, 
                   method = "pearson")
resAcc


#Eğtim ve Test seti oalrak ayır %80- %20
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(veri_kull$LoanAmountinK, SplitRatio = 0.8)
training_set = subset(veri_kull, split == TRUE)
test_set = subset(veri_kull, split == FALSE)
View(veri_kull)
View(training_set)
View(test_set)
#Özellik ölçeklendir
training_set_scaled = scale(training_set)
View(training_set_scaled)
test_set_scaled =scale(test_set)
View(test_set_scaled)


#Veri setine polinom regresyonu uygula (5. derece)

fit = lm(LoanAmountinK ~ poly(Income, 5), data = veri_kull)
coef(summary(fit))

fit2 = lm(LoanAmountinK ~ poly(Income, 5, raw = TRUE), data = veri_kull)
coef(summary(fit2))

# Gelirin min/macx değerlerini hesapla
incomeLims = veri_kull%>%
                select(Income)%>%
                range

# İlgili gelir aralığında gelir sırası yap seq
incomeGrid = seq(from = min(incomeLims), to = max(incomeLims))

# İlgili gelir verilerinden kredi miktarını uydur(fit) Standart hata kullanarak(SE)

preds = predict(fit, newdata = list(Income = incomeGrid), se = TRUE)
View(preds)
# Hata bandlarını hesapla (2*SE)
se_bands = cbind("ust" = preds$fit+2*preds$se.fit, 
                 "alt" = preds$fit-2*preds$se.fit)
head(se_bands)

#5. dereceden polinomu çiz ve tahmin et
ggplot() +
  #geom_point(data = veri_kull, aes(x = Income, y = LoanAmountinK)) +
  geom_line(aes(x = incomeGrid, y = preds$fit), color = "red") +
  geom_ribbon(aes(x = incomeGrid, 
                  ymin = se_bands[,"alt"], 
                  ymax = se_bands[,"ust"]), 
              alpha = 0.3) +
  xlim(incomeLims) +
  labs(title = "5. Derece Polinom Regresyon Değerleri")



preds2 = predict(fit2, newdata = list(Income = incomeGrid), se = TRUE)
# raw = true ile normal fit i karşılaştır.
head(abs(preds$fit - preds2$fit))

# Derece kararı ver   

fit_1 = lm(LoanAmountinK~Income, data = veri_kull)
fit_2 = lm(LoanAmountinK~poly(Income,2), data = veri_kull)
fit_3 = lm(LoanAmountinK~poly(Income,3), data = veri_kull)
fit_4 = lm(LoanAmountinK~poly(Income,4), data = veri_kull)
fit_5 = lm(LoanAmountinK~poly(Income,5), data = veri_kull)
fit_6 = lm(LoanAmountinK~poly(Income,6), data = veri_kull)
fit_7 = lm(LoanAmountinK~poly(Income,7), data = veri_kull)
print(anova(fit_1,fit_2,fit_3,fit_4,fit_5,fit_6,fit_7))


print(coef(summary(fit_5)))
