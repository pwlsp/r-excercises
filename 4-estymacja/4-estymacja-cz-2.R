##############################
### 4 kwietnia 2025 - laby ###
##############################

# Zadanie 7

# nie znamy rokładu zmiennej losowej
# Próba jest duża

n = 1200
srednia = 4.7
odchylenie = 2.2 ###### (zad8) ocena odchylenia standardowego

alfa = 0.05

library(BSDA) # to trzeba odpalać zawsze, ale nie trzeba już pobierać

zsum.test(srednia, odchylenie, n, conf.level = 0.95)

#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  4.575526 4.824474
#sample estimates:
#  mean of x 
#4.7

# z ufnością 95% przedział (4.57;4.83)
# pokrywa prawdziwą nieznaną średnią czasu
# trwania wszystkich połączeń telefonicznych




# Zadanie 8
dane = read.csv("/home/pwlsp/room/put/statystyka/4-estymacja/dane_est.csv",sep=";",dec=",")

diamenty = na.omit(dane$diamenty)

n = length(diamenty)
n

# ocena próbowa wariancji to będzie tutaj wariancja z próby

var=var(diamenty) # wariancja z próby
var
# 0.003081061

sd=sd(diamenty) # odchylenie standardowe z próby
sd
# 0.0555073

# "Następnie oszacuj dla danych z zadań 1-2, 4 oraz 7 odchylenia standardowe z 95% pewnością."

# wzór z wykładu

# left chi
Lchi = (n-1)*sd^2/qchisq(1-alfa/2, n-1)
Lchi
# 0.001546149

Pchi = (n-1)*sd^2/qchisq(alfa/2, n-1)
Pchi
# 0.00888205

# Left sd
Lsd=sqrt(Lchi)
Lsd
# 0.0393211
Psd=sqrt(Pchi)
Psd
# 0.09424463

install.packages("TeachingDemos")
library(TeachingDemos)

chi = sigma.test(diamenty, conf.level = 0.95)
chi

ci = chi$conf.int
ci
l = sqrt(ci[[1]])
l
# 0.0393211

p = sqrt(ci[[2]])
p
# 0.09424463

# Z ufnością 95% przedział (0.039;0.095)
# pokrywa prawdziwe nieznane odchylenie standardowe
# wagi syntetycznych diamentów.

## Teraaaz 8 dla zadania 7

var = odchylenie^2
var

Lchi = (n-1)*odchylenie^2/qchisq(1-alfa/2, n-1)
Lchi
Pchi = (n-1)*odchylenie^2/qchisq(alfa/2, n-1)
Pchi
# zaokrąglamy lewy w dół prawy w górę

Lsd = sqrt(Lchi)
Lsd
Psd = sqrt(Pchi)
Psd

# zad 11
n = 100
T = 4
phat = T/n # proporcja z próby
alfa = 0.05

L = phat - qnorm(1-alfa/2) * sqrt(phat*(1-phat)) / sqrt(n)
P = phat + qnorm(1-alfa/2) * sqrt(phat*(1-phat)) / sqrt(n)
L
# 0.001592707
P
# 0.07840729

# ( 0.15% ; 7.85% )

binom.test(T, n, conf.level = 0.95)
# (1.1% ; 9.93%)

prop.test(T, n, conf.level = 0.95)
# (1.28% ; 10.52%)

# wyniki są inne niż jakieś wyniki czegoś innego !!!!! DOWIEDZIEĆ SIĘ O CO CHODZI !!!

# Z ufnością 95% przedział (0.15% ; 7.85%) pokrywa
# prawdziwy nieznany odsetek niedopełnionych puszek.

# od 1.5% automat wymaga regulacji,
# nasz przedział to pokrywa, więc automat wymaga regulacji

# 9, 10, 12, 13 

# 12
n = 120
T = 24
phat = T / n
alfa = 0.1
binom.test(T, n, conf.level = 0.9)

# 13






