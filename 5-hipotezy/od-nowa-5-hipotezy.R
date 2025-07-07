dane = read.csv("dane_hip.csv", sep = ";", dec = ",")
dane
wiatr = na.omit(dane$wiatr)
pompa = na.omit(dane$pompa)
morze = na.omit(dane$morze)
blaszki = na.omit(dane$blaszki)
mleko = na.omit(dane$mleko)
kukulki = na.omit(dane$kukulki)
morze


#-------------------------------------------------------------------------------


##### ZADANIE 1
### Test hipotezy o średniej (mu)

{

# rozkład normalny
# "skonstruuj procedurę testującą"
alfa = 0.05

### krok 1

# H0: mu <= 4
# H1: mu > 4

### krok 2

mu0 = 4
srednia = mean(wiatr)
srednia
# to odchylenie standardowe z próby
S = sd(wiatr)
S
n = length(wiatr)
n
t = (srednia - mu0)/(S/sqrt(n))
t
# t = 2.4186 (zaokrąglamy normalnie)

### krok 3 - wyznaczenie obszaru krytycznego R

# kwantyl rozkładu t-studenta
qt(1-alfa, n-1)
# 1.795885

# R = (1.795885; inf)

### krok 4

# t należy do R -> odrzucamy H0

### krok 5
# Na poziomie istotności alfa = 0.05 dane potwierdzają hipotezę, że średnia
# prędkość wiatru przekracza 4m/s.
# Zatem okolice Darłowa nadają się do budowy elektrowni wiatrowej.


#-------------------------------------------------------------------------------


#### alternatywna procedura testowa ####

### krok 1
# H0: mu <= 4m/s      H1 m > 4m/s

### krok 2
t.test(wiatr, mu = mu0, alternative = "greater")

### krok 3 (wyznaczenie obszaru krytycznego R)

# p-value = 0.01705
# R = (1.795885 ; inf) co z tym?

### krok 4
# alpha = 0.05 > p-value = 0.01705 -> odrzucamy H0

### krok 5

# Odpowiedź:
# Na poziomie istotności alfa = 0.05 dane potwierdzają hipotezę, że średnia
# prędkość wiatru przekracza 4m/s.
# Zatem okolice Darłowa nadają się do budowy elektrowni wiatrowej.


#-------------------------------------------------------------------------------


# Wniosek:  Tutaj albo liczymy statystykę t (wzór)
#           oraz obszar krytyczny (kwantyl t_n-1) i je porównujemy,
#           albo liczymy p-value i porównujemy z alfą (lepszy sposób).

}


#-------------------------------------------------------------------------------


##### ZADANIE 2


{
pompa

# H0: mu >= 3.5
# H1: mu < 3.5

mu0 = 3.5

t.test(pompa, mu = mu0, alternative = "less")
# p-value = 0.1521
# alfa = 0.05

# alfa < p-value, czyli brak podstaw do odrzucenia H0

# Odpowiedź:
#   Na poziomie istotności alfa = 0.05, dane nie potwierdzają hipotezy,
#   że COP jest rzeczywiście znacznie mniejsze niż 3.5.

}

#-------------------------------------------------------------------------------


##### ZADANIE 3


{
library(BSDA)

# H0: mu = 870 m
# H1: mu != 870 m

morze

sig = 5
mu0 = 870

z.test(morze, sigma.x = 5, mu = 870, alternative = "two.sided")

# p-value = 0.6547
# alfa = 0.05

# alfa < p-value, czyli brak podstaw do odrzucenia H0

# Odpowiedź:
#   Na poziomie istotności alfa = 0.5 dane nie potwierdzają hipotezy,
#   że średnia głębokość morza w podanym rejonie jest różna od 870m.
}

#-------------------------------------------------------------------------------


##### ZADANIE 5
### Test hipotezy o wariancji (sigma^2)

{
### a

t.test(mleko, mu = 1.7, alternative = "two.sided")

# H0: mu = 1.7
# p-value = 0.1114
# alfa = 0.05

# alfa < p-value, czyli brak podstaw do odrzucenia H0
# czyli na poziomie istotności 0.05 dane nie potwierdzają, 
# że średnia zawartość tłuszczu w mleku wynosi 1.7%


### b

### sposób 1 (długi)

# krok 1
# H0: sig^2  >= 0.02 (%)^2     H1: sig^2  < 0.02 (%)^2

# krok 2
# statystyka testowa

n=length(mleko)
Chi2 = (n-1)*var(mleko)/0.02
Chi2
# Chi2 = 5.2

# 3
# jeżeli cośtam to obszar krytyczny jest od zera do kwantylu rozkładu Chi2
# obszar krytyczny (0 ; Chi_n-1,alfa)    ????


qchisq(alfa, n-1)
# R = (0; 3.325113)
# obszar krytyczny

# Chi2 nie należy do R, brak podstaw do odrzucenia H0

# Na poziomie istotności ... dane nie potwierdzają hipotezy, że
# wariancja tłuszczu jest mniejsza niż 0.02 %^2


### sposób 2 (krótki)

library(TeachingDemos)

sigma.test(mleko, sigmasq = 0.02, alternative = "less")
# p-value = 0.1835 
# alfa = 0.05
# alfa < p-value, czyli brak podstaw do odrzucenia H0
}

#-------------------------------------------------------------------------------

##### ZADANIE 7

{
# rozkład normalny
mu0 = 55 # mg/m3

sig0 = 18
sig20 = 18^2
sig20

n = 100
sr = 60
S = 20

alfa = 0.01

# test hipotez o mu
# H0: mu <= 55
# H1: mu > 55

library(BSDA)
zsum.test(sr, S, n, mu = mu0, alternative = "greater")
# z = 2.5
# p-value = 0.00621
# alfa = 0.01
# alfa > p-value, a zatem odrzucamy H0

chi2 = ((n-1)*S^2)/sig20
chi2
# chi2 = 122.22

# obszar krytyczny
qchisq(1-alfa, n-1)
# R = (134.6416; inf)

# chi2 nie należy do R, więc brak podstaw do odrzucenia H0
}


#-------------------------------------------------------------------------------

##### ZADANIE 8
### Test hipotezy o proporcji populacyjnej (p)

{
T = 1600 # liczba sukcesów
n = 2500 # liczba wszystkiego : )
phat = T/n
alfa = 0.05
p0 = 0.6

# Procedura testowa
# 1
# H0: p = 0.6    H1 p != 0.6

# 2
# statystyka testowa

z = (phat-p0)/sqrt(p0*(1-p0)/n)
z

# obszar krytyczny

# 3

qnorm(1-alfa/2)
# 1.959964

# R = (-inf, -1.959964) u (1.959964, +inf)


# Z należy do R   -> odrzucamy H0

# 5
# Na poziomie istotności 0.05 dane potwierdzają hipotezę,
# że 60% ogółu osób nie zamierza wziąć udziału w wyborach.

binom.test(T, n, p = 0.6, alternative = "two.sided")
# 4.413e-05

prop.test(T, n, p = 0.6, alternative = "two.sided")
# p-value = 4.864e-05 ~= 0
# alfa = 0.05

# alfa > p-value, czyli odrzucamy H0

}


##### ZADANIE 9

T = 16 # liczba sukcesów
n = 1200 # liczba wszystkiego : )
phat = T/n
alfa = 0.05
p0 = 0.02

# Procedura testowa
# 1
# H0: p >= 0.02    H1 p < 0.02

# 2
# statystyka testowa

z = (phat-p0)/sqrt(p0*(1-p0)/n)
z
# -1.649572

# obszar krytyczny

# 3

qnorm(1-alfa)
# 1.644854
# -> - 1.644854

# R = (-inf, -1.644854)

# Z należy do R   -> odrzucamy H0

# 5
# Na poziomie istotności 0.05 dane potwierdzają hipotezę,
# że frakcja jest mniejsza

binom.test(T, n, p = 0.02, alternative = "less")
# p-value = 0.05451

prop.test(T, n, p = 0.02, alternative = "less")
# p-value = 0.061
# alfa = 0.05

# alfa < p-value, czyli brak podstaw do odrzucenia H0

# dane nie potwierdzają na poziomie istotności

# inaczej ręcznie a inaczej automatycznie :0
# tak miało być, git
