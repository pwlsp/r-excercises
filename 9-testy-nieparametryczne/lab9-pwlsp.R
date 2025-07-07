#####################################################
#                                                   
#   Zadanie domowe: zrozumieć kiedy dawać exact=F   
#                                                   
#####################################################


##### ZADANIE 1

dane=read.csv("~/room/put/statystyka/statystyka-repo/9-testy-nieparametryczne/zadania/task_1.csv",sep=";",dec=",")
dane

### (jest to testowanie dwóch NIEZALEŻNYCH grup)

# mu1 - średnia ilość użyć maszynki z jednym ostrzem
# mu2 - ........................... z dwoma ostrzami
# H0: mu2 <= mu1
# H1: mu2 > mu1

single = dane$single
single
twin = dane$twin
twin

### Test U Manna-Whitneya

########## KOLEJNOŚĆ MA ZNACZENIE JAK JEST 'greater'

#wilcox.test(mu2 , mu1   , ...) mu2 to te które jest greater albo less
wilcox.test(twin, single, exact = F, alternative = 'greater')
### (jest to testowanie dwóch NIEZALEŻNYCH grup)

# p-value = 0.07767
# alfa = 0.05
# p-value > alfa
# nie ma podstaw do odrzucenia H0
# czyli dane (chyba) nie potwierdzają twierdzenia producenta podwójnego ostrza



##### ZADANIE 2

dane=read.csv("~/room/put/statystyka/statystyka-repo/9-testy-nieparametryczne/zadania/task_2.csv",sep=";",dec=",")
dane

plasticA = dane$plasticA
plasticB = dane$plasticB

plasticA
plasticB

# mu_1 - średnia wytrzymałość graniczna plastiku A
# mu_2 - średnia wytrzymałość graniczna plastiku B

# H0: mu_1 == mu_2
# H1: mu_1 != mu_2

wilcox.test(plasticA, plasticB, exact = F, alternative = 'two.sided')
# p-value jest takie samo niezależnie od kolejności
# p-value > alfa (0.06 > 0.05)
# zatem nie ma podstaw do odrzucenia H0
# Odp: Średnie wytrzymałości graniczne obu plastików są sobie równe.



##### ZADANIE 3
### (jest to testowanie dwóch ZALEŻNYCH grup)

### Teraz używamy też wilcox.test, ale pierwszy (i drugi) parametr są inne,
### przez co to jest teraz test rang.

dane = read.csv("~/room/put/statystyka/statystyka-repo/9-testy-nieparametryczne/zadania/task_3.csv",sep=";",dec=",")
dane

j83 = dane$January1983
j84 = dane$January1984

j83
j84

# H0: j83 == j84
# H1: j83 != j84

wilcox.test(j84 - j83, alternatice = "two.sided")
# p-value jest takie samo niezależnie od kolejności odejmowania

# p-value > alpha (0.4375 > 0.05)
# Zatem nie ma podstaw do odrzucenia H0.
# Średnie wartości wskażników nie różnią się.



##### ZADANIE 4

dane = read.csv("~/room/put/statystyka/statystyka-repo/9-testy-nieparametryczne/zadania/task_4.csv", sep=";", dec=",")
dane

before = dane$before
after = dane$after

before
after

# H0: mu2 <= mu1
# H1: mu2 > mu1    

wilcox.test(after-before, exact = F, alternative="less")
# tu kolejność ma znaczenie ofc

# p-value = 0.008577
# p-value < alfa   (0.008577 < 0.05)
# Odrzucamy H0.
# Zatem kampania odniosła sukces.



##### ZADANIE 5

### Test H Kruskalla-Wallisa
# lokalizacja

dane = read.csv("~/room/put/statystyka/statystyka-repo/9-testy-nieparametryczne/zadania/task_5.csv", sep=";", dec=",")
dane

line1 = dane$line1
line2 = dane$line2
line3 = dane$line3
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

# p-value = 0.02796
# p-value < alfa (0.05)
# Zatem odrzucamy H0.

# Dane wystarczają aby wskazać 
# różnice między lokalizacjami dla trzech zestawów danych.


# alternatywnie, ale to samo
wyniki = c(line1,line2,line3)
wyniki
n = length(line1)
obiekty=c(rep(1,n),rep(2,n),rep(3,n))
obiekty
kruskal.test(wyniki ~ obiekty)



##### ZADANIE 8 (na zajęciach przeskoczyliśmy)

dane = read.csv("~/room/put/statystyka/statystyka-repo/9-testy-nieparametryczne/zadania/task_8.csv", sep = ";", dec = ",")
dane

firma1 = dane$Bristol_Myers
firma2 = dane$Eli_Lilly
firma3 = dane$Pfizer
firma1
firma2
firma3

k1 = cbind(kat = 1:4, firma = 1, dane = firma1)
k2 = cbind(kat = 1:4, firma = 2, dane = firma2)
k3 = cbind(kat = 1:4, firma = 3, dane = firma3)
k1
# kat to po prostu kategoria chyba

daneX = as.data.frame(rbind(k1, k2, k3))
daneX
class(daneX)
friedman.test(daneX$dane, daneX$firma, daneX$kat)

# H0: skutki inflacji były odczuwalne w równym stopniu przez poszczególne firmy
# H1: Przynajmniej jedna para się różni

# p-value = 0.03877 < alfa = 0.05

# Odrzucamy H0
# Dane potwierdzają, że skutki inflacji były
# odczuwalne w różnym stopniu przez poszczególne firmy.



##### ZADANIE 9

dane = read.csv("~/room/put/statystyka/statystyka-repo/9-testy-nieparametryczne/zadania/task_9.csv", sep = ";", dec = ",")
dane

refs = dane$References
perf = dane$JobPerformance
refs
perf

# H0: siła referencji i pracy są ujemnie skorelowane,
#     współczynnik korelacji <= 0
# H1: siła referencji i pracy są dodatnio skorelowane,
#     współczynnik korelacji > 0

# (x, y)
cor.test(refs, perf, method="spearman", alternative = "greater")
# p-value = 0.1799 > alpha = 0.05
# brak podstaw do odrzucenia H0


### BONUS do zadania 9
cor(refs, perf)

plot(refs, perf)

prosta = lm(perf~refs)
prosta
b = prosta$coefficients[1]
b
a = prosta$coefficients[2]
a
abline(prosta, col="red")


##### ZADANIE 10

dane10 = read.csv("~/room/put/statystyka/statystyka-repo/9-testy-nieparametryczne/zadania/task_10.csv", sep = ";", dec = ",")
dane10

# H0: liczba opuszczonych godzin i roczne wynagrodzenie
#       nie są ze sobą powiązane -> korelacja wynosi 0

# H1: liczba opuszczonych godzin i roczne wynagrodzenie
#       są ze sobą powiązane -> korelacja nie równa się 0

cor.test(dane10$WorkHoursMissed, dane10$AnnulWages, method="spearman", alternative = "two.sided")

# p-value ~= 0 < alfa
# odrzucamy H0
# istnieje korelacja





