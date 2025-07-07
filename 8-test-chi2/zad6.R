# ZAD 4
library(nortest)
dane=read.csv("~/room/put/statystyka/statystyka-repo/8-test-chi2/normalnosc_punkty.csv",sep=";", dec=",")
dane

alfa=0.01

# testy normalności

# H0: dane mają rozkład normalny
# H1: dane nie mają rozkładu normalnego

ozon=dane$punkty
ozon

# pearsona
pearson.test(ozon, adjust = FALSE)
# p-value = 0.2689 > 0.05 = alfa -> brak podstaw do odrzucenia H0
pearson.test(ozon, adjust = TRUE)
# p-value = 0.146 > 0.05 = alfa -> brak podstaw do odrzucenia H0

# kołmogorowa-lillieforsa
lillie.test(ozon)
# p-value = 0.2774 > 0.05 = alfa -> brak podstaw do odrzucenia H0

# shapiro-wilka
shapiro.test(ozon)
# p-value = 0.1098 > 0.05 = alfa -> brak podstaw do odrzucenia H0

# cramera von misesa
cvm.test(ozon)
# p-value = 0.332 > 0.05 = alfa -> brak podstaw do odrzucenia H0

# andersona-darlinga
ad.test(ozon)
# p-value = 0.275 > 0.05 = alfa ->  brak podstaw do odrzucenia H0

# shapiro-francia (shapiro-wilka dla dużych prób)
sf.test(ozon)
# p-value = 0.08026 > 0.05 = alfa -> brak podstaw do odrzucenia H0

# Interpretacja: Na poziomie istotności 0.05 te dane mają rozkład normalny.


m=mean(ozon)
s=sd(ozon)
n=length(ozon)

p=seq(0,12,2) # -> 6 przedzialow
p
k=length(p)-1
serie = cut(ozon,p) # pokazuje, w ktorym przedziale znajdują sie kolejne wartości
serie
y = table(serie)
y

hist(ozon, p, freq=F)
curve(dnorm(x,m,s), xlim=c(p[1],p[k+1]),col='red', add=TRUE)

obserwacje=as.vector(y)
obserwacje
oczekiwane=c()

for(i in 1:k) {
    oczekiwane=c(oczekiwane, pnorm(p[i+1], m, s) - pnorm(p[i],m,s))
}
oczekiwane
sum(oczekiwane) # 0.9932772 < 1 
# przerabianie tak, aby suma całego prawdopodobieństwa wyszła 1
oczekiwane[1]=pnorm(p[2],m,s)
oczekiwane[k]=1-pnorm(p[k],m,s)
oczekiwane

sum(oczekiwane) # 1

# mniej niż 5 pomiarów -> sie sumuje

oczekiwane[k-1]=oczekiwane[k-1]+oczekiwane[k]
oczekiwane=oczekiwane[-k] # usuwanie ostatniej kolumny
oczekiwane

# same thing with obserwacje
obserwacje[k-1]=obserwacje[k-1]+obserwacje[k]
obserwacje=obserwacje[-k]

# k=k-1 -< nie wiem po co to tu było
chisq.test(obserwacje, p=oczekiwane)
# p-value = 0.92 > 0.05 = alfa -> brak podstaw do odrzucenia H0

# Interpretacja: Na poziomie istotności 0.05 stężenie ozonu ma rozkład normalny.
