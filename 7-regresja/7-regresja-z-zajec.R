# 7_Regresja.pdf

#-------------------------------------------------------------------------------

##### ZADANIE 1

dane=read.csv("~/room/put/statystyka/statystyka-repo/7-regresja/Reg_chemikalia.csv",sep=";",dec=",")
dane
dim(dane) # dimentions = podaje wymiary

### a)
plot(dane$surowiec, dane$produkt,
     xlab="Ilość użytego surowca",
     ylab="Ilość uzyskanego produktu",
     main="Zależność ilości produktu od użytego surowca") #scatter plot, czyli po prostu wykres punktowy

### b)

# kowariancja próbkowa
cov(dane$surowiec, dane$produkt) # kolejność obojętna chyba, ale (x, y)
# 138.4889

# Istnieje liniowa zależność, jest ona dodatnia,
# więc ze wzrostem ilości użytego surowca rośnie ilość uzyskanego produktu.

### c)

cor(dane$surowiec, dane$produkt)
# 0.8953468
# Jest bardzo silna korelacja (0.8-1.0)``

# Jeżeli kowariancja / współczynik korelacji są ujemne to po prostu zależność
# liniowa też jest ujemna.

### d) # ocena prostej regresji

prosta = lm(dane$produkt~dane$surowiec) # KOLEJNOŚĆ MA ZNACZENIE (y~x)
prosta

b0 = prosta$coefficients[1] # intercept, czyli przecięcie z osią y, czyli b
b0 
b1 = prosta$coefficients[2] # współczynnik przy x
b1 

# y = 22.4 + 3.62x

### e)

plot(dane$surowiec, dane$produkt,
     xlab="Ilość użytego surowca",
     ylab="Ilość uzyskanego produktu",
     main="Zależność ilości produktu od użytego surowca",
     pch = 20) # kropki

abline(prosta, col = "red")

### f)

1*b1
# o 3.619048 kg

### g)

prosta = lm((dane$produkt)~(dane$surowiec)) # TO NIE DZIAŁA WIĘC TRZEBA ZAMIENIĆ:
# nie wiemy czemu trzeba tutaj zamienić to:

surowiec = dane$surowiec
prosta = lm(dane$produkt~surowiec)

predict(prosta, data.frame(surowiec=20))
# wielkość produkcji wyniesie 94.79

### h) można zamiast 20 i 15 oddzielnie dać c(15,20)

predict(prosta, data.frame(surowiec=15))
# 76.69048

### i) 
###     dopasowanie do danych

res = summary(prosta)
res
res$r.squared # 0.8016458
# Model opisuje dane w 80%

### j)
###     weryfikacja testu o istotności regresji dla alfa = 0.05

# y = b0 + b1x
# H0: b1 = 0
# H1: b1 != 0

resAnova = anova(prosta)
alfa = 0.05
resAnova
pvalue = resAnova$`Pr(>F)`[1]
pvalue
if (pvalue<alfa) {
  print("Odrzucamy H0")
}

fvalue = resAnova$`F value`[1]
fvalue
n = length(surowiec)
kr = qf(1-alfa, 1, n-2) # kwantyl rozkladu
kr
# f-value > kr, a zatem odrzucamy H0

#-------------------------------------------------------------------------------


# ZADANIE 2

urzadzenie=read.csv("~/room/put/statystyka/statystyka-repo/7-regresja/Reg_urzadzenie.csv",sep=";",dec=",")
urzadzenie

plot(urzadzenie$efektywnosc, urzadzenie$zywotnosc,
     xlab = "Efektywność użytego surowca",
     ylab = "Żywotność użytego surowca",
     main = "Zależność")

# ZADANIE 3

arszenik=read.csv("~/room/put/statystyka/statystyka-repo/7-regresja/Reg_arszenik.csv",sep=";",dec=",")
arszenik

x = arszenik$pH
y = arszenik$arszenik

plot(x, y, main = "arszenik", pch=20, xlab = "Zakwaszenie", ylab= "ilosc arszeniku")

cov(x,y)
# wraz ze wzrostem zakwaszenia ilosc arszeniku maleje
cor(x,y)
# bardzo silny związek liniowy

lm(y~x)
# y = 190.27 - 18.03x

prosta = lm(y~x)
# zmienia się o b1 = - 18.03

predict(prosta, data.frame(x = c(7.5, 9)))
# dla 7.5 -- 55
# dla 9 -- 27

summary(prosta)

wynik = anova(prosta)
alfa = 0.01
wynik$`Pr(>F)`[1]
# Pr(<F) < alfa -- odrzucamy H0
# Regresja liniowa