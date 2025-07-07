# test zgodności z dowolnym rozkładem -> chi kwadrat

# ZAD 1

# H0: Rozkład zatrdunienia emerytów menadżerów w hrabstwie Allegheny odpowiada rozkładowi 
# badaniu stowarzyszenia
# H1: ~H0

obserwowane=c(122,85,76,17)
oczekiwane=c(0.38,0.32,0.23,0.07)
  
alfa=0.1

chisq.test(obserwowane, p=oczekiwane)

# alfa < p-value = 0.3845 -> brak podstaw do odrzucenia H0
# Interpretacja: Na poziomie istotności 0.1 ozkład zatrudnienia emerytowanych 
# menedzerów wyższego szczebla, którzy wrócili
# do pracy w hrabstwie Allegheny odpowiada badaniu stowarzyszenia.


# ZAD 2

# H0: Rozkład zgonów w pewnym okręgu nie różni się do rozkładu z badania
# H1: ~H0

obserwowane=c(68,27,5)
oczekiwane=c(0.74,0.16,0.1)

alfa=0.1

chisq.test(obserwowane, p=oczekiwane)
# p-value = 0.005121 < 0.1 = alfa -> odrzucamy H0

# Intrepretacja: Na poziomie istotności 0.1 rozkład zgonów w pewnym okręgu różni się od rozkładu z badania.


# ZAD 3 

# sumowanie dla każdego smaku - zaobserwowane wartosci
# każdy smak ma być 20% zawartości torebki

# H0: Wszystkie smaki stanowią 20% zawartości torebki.
# H1: ~H0

obserwowane=c(7+20+4+12, 20+5+16+9, 10+5+13+16, 7+13+21+3, 14+17+4+17)
oczekiwane=rep(0.2, 5)

alfa=0.05

chisq.test(obserwowane, p=oczekiwane)
# p-value = 0.8369 > 0.05 = alfa -> brak podstaw do odrzucenia H0

# Interpretacja: Rozkład smaków jest taki jak podany - wszystkich smaków jest po równo (każdy smak stanowi 20% zawartości torebki).


# ZAD 4
library(nortest)
dane=read.csv("~/room/put/statystyka/statystyka-repo/8-test-chi2/normalnosc_ozon.csv",sep=";", dec=",")
dane

alfa=0.05

# testy normalności

# H0: dane mają rozkład normalny
# H1: dane nie mają rozkładu normalnego

ozon=dane$ozon
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
oczekiwane

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


### do egzaminu
# Your data
oczekiwane <- c(0.068741338, 0.244550431, 0.382434716, 0.238858688, 0.059370196, 0.006044631)

# Example bin midpoints (adjust as needed)
x <- seq(1, length(oczekiwane))

# Make a histogram-like bar plot
barplot(
    oczekiwane,
    space = 0,      # no space between bars
    names.arg = x,  # x-axis labels
    xlab = "ozon",
    ylab = "Density",
    col = "grey",
    border = "black"
)
###



# ZAD 5 DOK

# ZAD 6 DOK



# ZAD 7 badanie niezależności 

# H0: zmiennne niezależne
# H0: Socjolog NIE może stwierdzić, że miejsce zamieszkania danej osoby zależy od liczby lat studiów. -> niezależność zmiennych
# H1: Socjolog może stwierdzić, że miejsce zamieszkania danej osoby zależy od liczby lat studiów.

alfa=0.05

# tablica kontyngencji
miejski=c(15,12,8)
podmiejski=c(8,15,9)
wiejski=c(6,8,7)

tablica=data.frame(miejski, podmiejski, wiejski)
tablica

chisq.test(tablica)
# p-value = 0.5569 > 0.05 = alfa -> brak podstaw do odrzucenia H0

# Interpretacja: Na poziomie istotności 0.05 można stwierdzić, że te zmienne są od siebie niezależne,
# więc socjolog nie moze stwierdzić, że miejsce zamieszkania danej osoby zależy od liczby lat studiów.


# ZAD 8

# H0: Zmienne niezależne: odsetek pasażerów, którzy zgubili bagaż w trakcie lotum, NIE zależy od lini lotniczej.
# H1: ~H0``

alfa=0.05

linia1=c(10,90)
linia2=c(7,93)
linia3=c(4,96)

tak=c(10,7,4)
nie=c(90,93,96)

tab1=data.frame(linia1, linia2, linia3)
chisq.test(tab1)

tab2=data.frame(tak, nie)
chisq.test(tab2)

# Wniosek: niezależnie w którą stronę zrobimy, wychodzi takie samo p-value.

# p-value = 0.251 > 0.05 = alfa -> brak podstaw do odrzucenia H0.

# Interpretacja: Na poziomie istotności 0.05 te zmienne są od siebie niezależne, więc
# odsetek pasażerów, którzy zgubili bagaż w trakcie lotu, nie zależy od linii lotniczej.



# ZAD 9

# H0: Zmienne niezależne: opinia nie zależy od wieku.
# H1: Opinia zależy od wieku.

alfa=0.05

za=c(96,96,90,36)
przeciw=c(201,189,195,234)
nieWiem=c(3,15,15,30)

tabela=data.frame(za,przeciw,nieWiem)
chisq.test(tabela)

# bardoz małe p-value < alfa -> odrzucamy H0

# Interpretacja: Na poziomie istotności 0.05 opinia zależy od wieku.


install.packages("nortest")
library(nortest)