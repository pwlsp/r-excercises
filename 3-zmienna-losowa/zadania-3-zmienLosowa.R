################################################################################
#Zadanie 1 - rozkład dwumianowy

#5 oznacza liczbę zanieczyszczonych studni spośród wybranych

#(a)

#S ma rozkład dwumianiwy
n = 5
p = 0.3
x = 0:5
pr=dbinom(x,n,p)
rbind(x,pr)
#e = rbind(x,pr)
plot(x,pr,type="h", lwd=4, col="lightblue", main="Histogram rozkładu dwumianowego",xlab="Ilość zanieczyszconych studni",ylab="Prawdopodobieństwo")
#btw tak można się dostać do komórki e["pr",3]

#(b)

#(i)

dbinom(3,n,p) #d - funkcja gęstości
#P(S=3) = 0.1323

#(ii)

#P(S>=3) = P(S=3) + P(S=4) + P(S=5) = 0.16308
dbinom(3,n,p)+dbinom(4,n,p)+dbinom(5,n,p)
#P(S>=3) = 1 - P(S<3) = 1 - P(S<=2) = 1 - F(2) = 0.16308
1 - pbinom(2,n,p) #p - dystrybuanta

#(iii)

#P(S<3) = P(S>=2) = F(2) = 0.83692
pbinom(2,n,p)
dbinom(0,n,p) + dbinom(1,n,p) + dbinom(2,n,p)




################################################################################
#Zadanie 2 - rozkład dwumianowy
n = 8
p = 0.9
x = 0:8
pr=dbinom(x,n,p)
rbind(x,pr)

dbinom(8,n,p)
#P(B=8) = 0.4304672

dbinom(7,n,p)
#P(B=7) = 0.3826375

1 - pbinom(5,n,p)
#P(B>5) = 1 - F(5) = 0.961908po2

EX = sum(x*pr)
EX
# E(B) = 7.2
# Interpretacja:
# możemy spodziewać się, że przeciętnie
# liczba żarówek świecących ponad 500 godzin
# będzie wynosić 7

#SD(B)
var = sum(x^2*pr)-EX^2
var
SD = sqrt(var)
SD
# SD(B) = 0.8485281
# SD - odchylenie standardowe (standard deviation) 
#
# ODP:
# Mozemy spodziewać się, ze przeciętnie
# liczba żarówek świecących przez ponad 500 godzin
# będzie odchylała sie od średniej o 1 żarówkę

#Zadanie 3 - rozkład wykładniczy
lambda = 0.01
curve(dexp(x,lambda),0,800)

#P(x>200) = 1- F(200) = 0.1353353
1-pexp(200,lambda)

#P(X<100) = F(100) = 0.6321206
pexp(100,lambda)

#P(X<500) = P(X<=100) = F(500) = 0.9932621
pexp(500,lambda)

#Zadanie 4 - rozkład wykładniczy
EX = 2.4
lambda = 1/EX
curve(dexp(x,lambda),0,15)

#P(X>3) = 1- P(X<=3) = 1 - F(3) = 0.2865048
1 - pexp(3,lambda)

#P(2<=X>=3) = F(3) - F(2) = 0.1480934
pexp(3,lambda) - pexp(2,lambda)

#Zadanie 5 - rozkład normalny
mu = 0.13
sig = 0.005

curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig)

#P(0.12<x<0.14) = F(0.14) - F(0.12) = 0.9544997
pnorm(0.14,mu,sig) - pnorm(0.12,mu,sig)

#Zadanie

#Jeżeli próba jest duża (n>30) to rozkład
#dwumianowy możemy przybliżyć rozkładem normalnym
#X~app N(n*p,sqrt(n*p*(1-p)))

#P(X<=15) F(15) =
pnorm(15,n*p,sqrt(n*p*(1-p)))

################################################################################
# Zadanie 9

# x ma rozkład normalny
mu=200
sig=10
n= 25

### (a)
# Średnia z próby AVR~(mu,sig/sqrt(n))
# P(199<AVR<202) = F(202) - F(199)

pnorm(202,mu,sig/sqrt(n)) - pnorm(199,mu,sig/sqrt(n))
# odp: P(199<AVR<202) = F(202) - F(199) = 0.5328072

### (b)
# T = X_1+X_2+...+X_25 ~ N(mu*n,sqrt(n)(sig))

#P(T <= 5100) = F(5100)

pnorm(5100, mu*n, sqrt(n)*sig)

################################################################################
# Zadania 10 i 11 do chaty

################################################################################
# Przykład 5 z wykładu 3 (W3_RozkladyStatystyk.pdf)

p = 0.25
T = 232
n = 1000
phat = T / n # p z daszkiem -- proporcja z próby
# phat ~ N(p, sqrt(p*(1-p)/n))

# P(phat <= 232/1000) = F(232/1000)

pnorm(232/1000, p, sqrt(p*(1-p)/n))
# odp: 0.09433326













