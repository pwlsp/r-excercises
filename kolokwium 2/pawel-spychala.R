# 160157
# Paweł Spychała

##### ZADANIE 1

T = 300 # liczba sukcesów
n = 500 # liczba wszystkiego
phat = T/n
alfa = 0.07
p0 = 0.46

# Procedura testowa
# 1
# H0: p == 0.46    H1 p != 0.46

prop.test(T, n, p = 0.46, alternative = "two.sided")

# p-value = 4.482e-10 ~= 0
# alfa = 0.05

# alfa > p-value, czyli odrzucamy H0

# Na poziomie istotności alfa=0.05 dane potwierdzają, że studenci danej uczelni
# zachowują się inaczej niż przeciętny Polak.




##### ZADANIE 2

technologia=read.csv("~/room/put/statystyka/statystyka-repo/kolokwium 2/ANOVA_technologia.csv",sep=";", dec=",")
technologia

### a)
# analiza wariancji

# H0: sig1^2 = sig2^2 = sig3^2 = sig4^2 
# H1: ~H0

metoda=rep(names(technologia),c(length(na.omit(technologia$T1)),
                              length(na.omit(technologia$T2)),
                              length(na.omit(technologia$T3))))
wyniki=c(na.omit(technologia$T1),
         na.omit(technologia$T2),
         na.omit(technologia$T3))
metoda
wyniki

#zamiast tabelki jest wszystko w pionie: wyniki-metoda
technologiaTest=data.frame(wyniki,metoda)
technologiaTest

bartlett.test(wyniki~metoda,technologiaTest)

# p-value = 0.1766
# alfa = 0.05
# alfa < p-value -> brak podstaw do odrzucenia H0

# Interpretacja: Na poziomie istotności 0.05 nie mamy podstaw do odrzucenia 
# hipotezy o jednorodności wariancji.
# Wniosek: Możemy przeprowadzić ANOVE

#-------

# H0: brak istotnych różnic między średnimi
#       zawartościami ołowiu: mu1 = mu2 = mu3
# H1: ~H0

model=lm(wyniki~metoda)
anova(model)

alfa=0.05

# Pr(>F) to to samo, co p-value
# p-value = 4.708e-11 < 0,05 = alpha -> odrzucamy H0

# Interpretacja: Na poziomie istotności 0,05 dane potwierdzają hipotezę o
#                   braku istotnych różnic mięzdy rzeczywistymi średnimi zawartościami ołowiu we krwi
#                   wśród pracowników zakładu w zależności technologii produkcji

### b)

TukeyHSD(aov(wyniki~metoda), ordered=TRUE)

# I grupa jednorodna {T1, T2}




##### ZADANIE 3

tlendane = read.csv("~/room/put/statystyka/statystyka-repo/kolokwium 2/Reg_tlen.csv",sep=";", dec =",")
tlendane

temperatura = tlendane$temperatura
tlen = tlendane$tlen
temperatura
tlen

### a)
cor(temperatura, tlen)
# Współczynnik korelacji wynosi -0.8675324
# A zatem zależność liniowa między temperaturą a zawartością tlenu w wodzie destylowanej
# istnieje, jest ujemna i bardzo silna.

### b)
plot(temperatura, tlen, pch=20, xlab = "Temperatura", ylab = "Zawartość tlenu", main = "Zależność między temperaturą a zawartością tlenu")

prosta = lm(tlen~temperatura)
prosta
b = prosta$coefficients[1]
b
a = prosta$coefficients[2]
a

# y = 14.50381 + -0.2510463x

abline(prosta)

# Wzroścnie o 3.62 kg

### c)
predict(prosta, data.frame(temperatura = 15))
# przewidywana ilość tlenu wynosi 10.73812 O2/dm^3


##### ZADANIE 4

# ZAD 1

# H0: Odsetek odpowiada przedstawionym w artykule
# H1: Odsetek różni się

n = 70
obserwowane=c(25,21,20,4)
oczekiwane=c(0.35,0.32,0.18,0.15)

alfa=0.07

chisq.test(obserwowane, p=oczekiwane)

# alfa > p-value = 0.03728 -> odrzucamy H0
# Interpretacja: Na poziomie istotności 0.07 odsetek mężczyzn o danych kolorach
# w badaniu badacza różni się od tyc umieszczonych w artykule


##### ZADANIE 5


dane = read.csv("~/room/put/statystyka/statystyka-repo/kolokwium 2/soki.csv", sep=";", dec=",")
dane

line1 = dane$firma1
line2 = dane$firma2
line3 = dane$firma3
line1
line2
line3

# H0: mu1 == mu2 == mu3
# H1: mu1 != mu2 lub mu1 != mu3 lub mu2 != mu3

dane1 = cbind(data=line1, group=1)
dane2 = cbind(data=line2, group=2)
dane3 = cbind(data=line3, group=3)
dane1
dane2
dane3

daneX = rbind(dane1, dane2, dane3)
daneX

kruskal.test(data ~ group, data = daneX)

# p-value = 0.02387
# p-value < alfa (0.05)
# Zatem odrzucamy H0.

# Dane wystarczają aby wskazać 
# różnice między lokalizacjami dla trzech zestawów danych produkcyjnych przy
# poziomie istotności 5%.



