# hipoteza z równością zawsze ma być w H0, nie w H1

dane = read.csv("dane_hip.csv",sep=";",dec=",")
dane

############### zadanie 1 ############### 

wiatr = na.omit(dane$wiatr)
# procedura testowa

# 1
# H0: mu <= 4m/s      H1 m > 4m/s

# 2
srednia = mean(wiatr)
mu0 = 4
S = sd(wiatr)
n = length(wiatr)
t = (srednia - mu0)/(S/sqrt(n))
t
# t = 2.418622

# 3 (wyznaczenie obszaru krytycznego R)

# kwantyl rozkładu t-studenta                                     <- ? !

alfa = 0.05
qt(1-alfa, n-1)
# R = (1.795885 ; inf)

# 4
# t należy do R -> odrzucamy H0

# 5
# Na poziomie istotności alfa = 0.05 dane potwierdzają hipotezę, że średnia
# prędkość wiatru przekracza 4m/s.
# Zatem okolice Darłowa nadają się do budowy elektrowni wiatrowej.

#### alternatywna procedura testowa ####

# 1
# H0: mu <= 4m/s      H1 m > 4m/s

# 2
t.test(wiatr, mu = mu0, alternative = "greater")

# 3 (wyznaczenie obszaru krytycznego R)

# p-value = 0.01705
# R = (1.795885 ; inf) co z tym?

# 4
# alpha = 0.05 > p-value = 0.01705 -> odrzucamy H0

# 5
# Na poziomie istotności alfa = 0.05 dane potwierdzają hipotezę, że średnia
# prędkość wiatru przekracza 4m/s.
# Zatem okolice Darłowa nadają się do budowy elektrowni wiatrowej.



############### Zadanie 3 ############### 

morze = na.omit(dane$morze)
odchylenie = 5 # znamy odchylenie standardowe


# 1
# H0: mu = 870m         H1: mu != 870m

# 2
# statystyka testowa z

# ZAD DOM, zrobić to drugą metodą                                   !!!!!

library(BSDA)
z.test(morze, sigma.x=odchylenie, mu = 870, alternative = "two.sided")

# 3
# p-value = 0.6547

# 4
# alpha = 0.05    <    p-value = 0.6547   ->  brak podstaw do odrzucenia H0

### skąd bierze się alfę?

# Na poziomie istotności alfa = 0.05 dane nie potwierdzają hipotezy,
# że średnia głębokość morza w tym rejonie jest różna od 870m.



############### zadanie 4 ############### 

blaszki = na.omit(dane$blaszki)

# H0: mu <= 0.04 mm     H1: mu > 0.04 mm

# Rozkład nie jest znany, próba duża

# alfa = 0.02
library(BSDA)
zsum.test(mean(blaszki), sd(blaszki), length(blaszki),
          mu = 0.04, alternative = "greater", conf.level = 0.98)

# 3
# p-value = 0.05041

# 4
# alfa = 0.02    <    p-value = 0.05041     -> brak podstaw do odrzucenia H0

# 5
# Na poziomie istotności alfa = 0.05 dane nie potwierdzają hipotezy,
# że produkowane przez ten automat blaszki są grubsze niż nominalna grubość.

############### zadanie 5 ############### 

mleko = na.omit(dane$mleko)
mleko

alfa = 0.05

## a) ##

# 1
# H0: mu = 0.017      H1: mu != 0.017

# 2

mu0 = 1.7
t.test(mleko, mu = mu0, alternative = "two.sided")
# t = -1.765   p-value = 0.1114

# 3 (wyznaczenie obszaru krytycznego R)

# p-value = 0.1114

# 4
# alpha = 0.05 < p-value = 0.1114 -> brak podstaw do odrzucenia H0

# 5
# Na poziomie istotności alfa = 0.05 dane nie potwierdzają hipotezy,
# że średnia zawartość tłuszczu w mleku wynosi 1,7 %.

## b) ##
# PROCEDURA TESTOWA

# 1
# H0: sig^2  >= 0.02 (%)^2     H1: sig^2  < 0.02 (%)^2

# 2
# statystyka testowa

n=length(mleko)
Chi2 = (n-1)*var(mleko)/0.02
Chi2
# Chi2 = 5.2

# 3
# jeżeli cośtam to obszar krytyczny jest od zera do kwantylu rozkładu Chi2
# obszar krytyczny (0 ; Chi_n-1,alfa)    ????

qchisq(alfa, n-1)
# R = (0;3.325113)

# 4
# Chi2 nie należy do R, więc brak podstaw do odrzucenia H0.

#5
# Na poziomie istotności alfa = 0.05
# dane nie potwierdzają hipotezy, że wariancja
# zawartości tłuszczu w mleku jest mniejsza niż 0,02 (%)^2

### Procedura testowa w R

alfa = 0.05
# 1

# H0: sig^2  >= 0.02 (%)^2     H1: sig^2  < 0.02 (%)^2

# 2
library(TeachingDemos)
sigma.test(mleko, sigmasq = 0.02, alternative = "less")

# 3
# p-value = 0.1835

# 4
# alfa = 0.05 < p-value = 0.1835      ->      brak podstaw do odrzucenia H0

# Na poziomie istotności alfa = 0.05
# dane nie potwierdzają hipotezy, że wariancja
# zawartości tłuszczu w mleku jest mniejsza niż 0,02 (%)^2



######## Zadanie 8 #########

T = 1600
n = 2500
phat = T/n
alfa = 0.05
p0 = 0.6

# Procedura testowa
# 1
# H0: p = 0.6    H1 p != 0.6

# 2 statystyka testowa

z = (phat-p0)/sqrt(p0*(1-p0)/n)

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

