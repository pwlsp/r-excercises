# slajd 13? przykłd 1 chyba

typ1=c(2830, 2840, 2800, 2880, 2820)
typ2=c(2790,2720,2770,2780,2760)

t.test(typ1,typ2,var.equal = TRUE,conf.level = 0.99)

#	Two Sample t-test
#data:  typ1 and typ2
#t = 3.9009, df = 8, p-value = 0.004539
#alternative hypothesis: true difference in means is not equal to 0
#99 percent confidence interval:
#  9.789727 130.210273
#sample estimates:
#  mean of x mean of y 
#2834      2764 

# Z ufnością 99% przedział od 9.78h do 130.22h
# pokrywa różnicę prawdziwych średnich żywotności
# wszystkich żarówek dwóch typów.


### KOMENTARZ
### Na slajdzie 7/33 mi_0 jest naszą hipotetyczną różnicą między średnimi,
### więc można nie tylko, "czy średni wzrost mężczyzn jest większy od śred. kobiet?"
### ale też "średni wzrost M jest większy o 10 cm od śr. wzr. kobiet."

## Wracamy do zadania z żarówkami:

# H0: mu1-mu2 <= 0   H1: mu1-mu2 > 0
# równe wariancje
t.test(typ1,typ2,var.equal=T,mu=0,alternative = "greater")

Two Sample t-test

### OUTPUT:
#data:  typ1 and typ2
#t = 3.9009, df = 8, p-value = 0.002269
#alternative hypothesis: true difference in means is greater than 0
#95 percent confidence interval:
#  36.6316     Inf
#sample estimates:
#  mean of x mean of y 
#2834      2764 


## czym jest "p-wartość"
# alpha = 0.01  >  pval=0.0002     -->   odrzucamy H0

## Interpretacja:
## Na poziomie istotności 1% dane potwierdzają,
## że średnia żywotność żarówek 1 typu
## jest większa niż żarówek 2 typu



## !!!!!! Wariancję z populacji oznaczamy sigma^2
## !!!!!! Wariancję z próby oznaczamy S^@



### slajd 14/33 przykład 1 - c.d.

# H0: sigma1^2 = sigma2^2        H1: sigma1^2 != sigma2^2

# "rozkład wariancji z próbki to rozkład chi^2"               !!!! ogarnąć

# rozkład "~ F-Snedecora"
# rozkłady t i chi^2 miały jeden parametr, F-Snedecora ma dwa parametry

install.packages("PairedData")


# podobno ten iloraz S/S z slajdu 17
# to sprawdzamy czy iloraz = 1 (iloraz Sów? Sigm?)

# H0: sigsq1 = sigsq2    H1: sisq1 != sigsq2

var.test(typ1, typ2)
#	F test to compare two variances

#data:  typ1 and typ2
#F = 1.2055, num df = 4, denom df = 4, p-value = 0.8607
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  0.1255116 11.5780634
#sample estimates:
#  ratio of variances 
#1.205479 

# alpha = 0.02 < 086=pval    -> brak podstaw do odrzucenia H0

var.test(typ1, typ2, conf.level = 0.99)
# 99 percent confidence interval:
# 0.05206242 27.91227571

# 1 należy do powyższego przedziału

# czym jest p z daszkiem !?!?!??!??!? XDDDDDDDD

# slajd 21: tutaj binom.test już nie zadziała i trzeba prop.test

## "T" to chyba liczba sukcesów

# "z" z przedziałów ma coś wspólnego z rozkładem normalnym






# przykład 2
nAW = 150
nT = 100
tAW = 107
tT = 63
prop.test(c(tAW, tT), c(nAW, nT), conf.level = 0.95)
#data:  c(tAW, tT) out of c(nAW, nT)
#X-squared = 1.551, df = 1, p-value = 0.213
#alternative hypothesis: two.sided
#95 percent confidence interval:
#  -0.04412743  0.21079410
#sample estimates:
#  prop 1    prop 2 
#0.7133333 0.6300000 
 
# z ufnością 95% przedział od -0.04 do 0.22 pokrywa nieznaną różnicę proporcji
# populacyjnych w nauczaniu technikami audiowiz. i technikami tradycyjnymi


# H0: pAW <= pT    H1: pAW > pT


prop.test(c(tAW, tT), c(nAW, nT), alternative = "greater")
# alpha = 0.05   <   0.1 = pval    --> nie odrzucamy H0

# Na poziomie istotności 5% dane nie potwierdzają,
# że nowa metoda nauczania daje istotnie większą zdawalność.



# Na slajdzie 25 nie możemy użyć poprzednich technik, bo dwie populacje
# są zależne od siebie (ci sami pacjenci)

# Trzeba zrobić z różnicę dla każdego pacjenta i zrobić jedną populację (różnic)

# sigma nieznana -> no to t-studenta (o co chodzi looool)


#d = (dane z prezki (różnice wartości))
t.test(d)

# H0: muD <= 0     H1: muD > 0
t.test(d, alternative = "greater")

# alpha = 0.05    >    0.003 = pval     ->  odrzucamy H0

# Dane potwierdzają, że testowana tabletka istotnie obniża ciśnienie.
