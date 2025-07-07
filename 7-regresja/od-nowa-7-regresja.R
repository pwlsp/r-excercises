##### ZADANIE 1
chemikalia = read.csv("~/room/put/statystyka/statystyka-repo/7-regresja/Reg_chemikalia.csv",sep=";")
chemikalia

surowiec = chemikalia$surowiec
produkt = chemikalia$produkt

### a)
plot(surowiec, produkt, pch=20, xlab = "Ilość zużytego surowca", ylab = "Końcowa ilość produktu", main = "Zależność między ilością zużytego surowca a ilością produktu")

### b)
cov(surowiec, produkt)
# Kowariancja wynosi 138.4889
# A zatem istnieje dodatnia liniowa zależność.
# Więc ze wzrostem użytego surowca wzrasta ilość produktu.

### c)
cor(surowiec, produkt)
# Współczynnik korelacji wynosi 0.8953468
# A zatem zależność liniowa jest bardzo silna.

### d)
prosta = lm(produkt~surowiec) # KOLEJNOŚĆ MA ZNACZENIE (y~x)
prosta
b = prosta$coefficients[1]
b
a = prosta$coefficients[2]
a

# y = 22.4 + 3.62x

### e)
abline(prosta)

### f)
# Wzroścnie o 3.62 kg

### g)
predict(prosta, data.frame(surowiec = 20))

### h)
predict(prosta, data.frame(surowiec = 15))

### i)
###     dopasowanie do danych

res = summary(prosta)
res
res$r.squared # 0.8016458
# Model opisuje dane w 80%

### j)
# y = b0 + b1x
# H0: b1 = 0
# H1: b1 != 0

### sposób 1
resAnova = anova(prosta)
alfa = 0.05
resAnova
# p-value = 0.0004617 < 0.05
#p-value < alfa, a zatem odrzucamy H0

# ODPOWIEDŹ: Na poziomie istotności alfa = 0.05 regresja liniowa jest istotna.


#### ZADANIE 2

urzadzenie = read.csv("~/room/put/statystyka/statystyka-repo/7-regresja/Reg_urzadzenie.csv",sep=";")
urzadzenie

efektywnosc = urzadzenie$efektywnosc
zywotnosc = urzadzenie$zywotnosc
efektywnosc
zywotnosc

### a)

plot(efektywnosc, zywotnosc, pch = 20,
     xlab = "Efektywność urządzenia", ylab = "Żywotność urządzenia",
     main = "Zależność żywotności od efektywności urządzenia")

### b)

cov(efektywnosc, zywotnosc)
# kowariancja = -8.652778
# a więc istnieje zależność liniowa ujemna
# ze wzrostem efektywności, maleje żywotność

### c)

cor(efektywnosc, zywotnosc)
# współczynnik korelacji = -0.9094164
# a więc związek liniowy jest bardzo silny

### d)
###     wyznacz ocenę prostej regresji


prosta = lm(zywotnosc~efektywnosc) # KOLEJNOŚĆ MA ZNACZENIE (y~x)
prosta
b = prosta$coefficients[1]
b
a = prosta$coefficients[2]
a

# ocena prostej regresji liniowej:
# y = 18.88227 - 0.8628809x

### d2)
###     dodaj do wykresu prostą regresji

plot(efektywnosc, zywotnosc, pch = 20,
     xlab = "Efektywność urządzenia", ylab = "Żywotność urządzenia",
     main = "Zależność żywotności od efektywności urządzenia")
abline(prosta, col="red")

### e)
###     Jak zmieni się żywotność urządzenia?

a
# Zmaleje o 0.8628809 miesiąca

### KOMENTARZ: Pamiętać, żeby napisać czy wzrośnie czy zmaleje i o ile *czego*.

### f)

predict(prosta, data.frame(efektywnosc = 11))
# żywotność = 9.390582

### g)

predict(prosta, data.frame(efektywnosc = 19))
# żywotność = 2.487535

### h)
###     dopasowanie do danych

res = summary(prosta)
res
res$r.squared # 0.8270381
# Model opisuje dane w 83%

### i)
###     weryfikacja testu o istotności regresji dla alfa = 0.05

# y = b0 + b1x
# H0: b1 = 0
# H1: b1 != 0

### sposób 1
resAnova = anova(prosta)
alfa = 0.05
resAnova
# p-value = 0.0006735 < 0.05
pvalue = resAnova$`Pr(>F)`[1]
pvalue
if (pvalue<alfa) {
    print("Odrzucamy H0")
}
# p-value < alfa, a zatem odrzucamy H0

# ODPOWIEDŹ: Na poziomie istotności alfa = 0.05 regresja liniowa jest istotna.

### sposób 2
fvalue = resAnova$`F value`[1]
fvalue
n = length(surowiec)
kr = qf(1-alfa, 1, n-2) # kwantyl rozkladu
kr
# f-value > kr, a zatem odrzucamy H0


#-------------------------------------------------------------------------------


##### ZADANIE 3

dane_arszenik = read.csv("~/room/put/statystyka/statystyka-repo/7-regresja/Reg_arszenik.csv", sep = ";", dec = ",")
dane_arszenik

pH = dane_arszenik$pH
pH

arszenik = dane_arszenik$arszenik

### a)
###     Narysuj diagram punktowy

plot(pH, arszenik, pch = 20,
     xlab = "zakwaszenie gleby", ylab = "% ilość arszeniku",
     main = "zależność między zakwaszeniem gleby a ilością usuniętego arszeniku")

### b)
###     Oblicz i zinterpretuj kowariancję i współczynnik korelacji

cov(pH, arszenik) # kowariancja wynosi -18.32216
cor(pH, arszenik) # współczynnik korelacji wynosi -0.9504953

# Interpretacja:
#               Istnieje ujemna zależność liniowa między pH a ilością arszeniku.
#               Związek liniowy jest bardzo silny.

### c)

prosta = lm(arszenik~pH)
prosta

b = prosta$coefficients[1]
b
a = prosta$coefficients[2]
a

# ocena prostej regresji liniowej:
# y = b + ax
# y = 190.27 - 18.03

### d)

a

# Odpowiedź: Ilość usuniętego arszeniku zmaleje o 18.03%.

### e)

### f)

predict(prosta, data.frame(pH = 7.5))
# usuniętego arszeniku = 55.01145 %

### g)

predict(prosta, data.frame(pH = 9))
# usuniętego arszeniku = 27.96008 %

### h)
###     dopasowanie do danych

res = summary(prosta)
res
res$r.squared # 0.9034413
# Model opisuje dane w 90%

### i)
###     weryfikacja testu o istotności regresji dla alfa = 0.01

# y = b0 + b1x
# H0: b1 = 0
# H1: b1 != 0

### sposób 1
resAnova = anova(prosta)
alfa = 0.01
resAnova
# p-value = 1.552e-09 < 0.05
pvalue = resAnova$`Pr(>F)`[1]
pvalue
if (pvalue<alfa) {
    print("Odrzucamy H0")
}
# p-value < alfa, a zatem odrzucamy H0

# ODPOWIEDŹ: Na poziomie istotności alfa = 0.01 regresja liniowa jest istotna.

### sposób 2
fvalue = resAnova$`F value`[1]
fvalue
n = length(surowiec)
kr = qf(1-alfa, 1, n-2) # kwantyl rozkladu
kr
# f-value > kr, a zatem odrzucamy H0