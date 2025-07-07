
#ANOVA może zostać przeprowadzona tylko w przypadku jednorodności wariancji


# ZAD 1 check normalność

cisnienie=read.csv("Anova_cisnienie.csv",sep=";")
cisnienie

alpha=0.05

# analiza wariancji
# test równości wariancji = test jednorodności wariancji

# H0: sig1^2 = sig2^2 = sig3^2 = sig4^2
# H1: ~H0

# do każdej komórki z tabelki przyporządkowuje jakie było ciśnienie
metoda=rep(names(cisnienie),each=length(cisnienie$Niskie))
metoda

# wypisuje wszystkie obserwacje po kolei od niskich do bardzo silnych w takiej kolejnosci
wyniki=c(cisnienie$Niskie,cisnienie$Srednie,cisnienie$Silne,cisnienie$BardzoSilne)
wyniki

#zamiast tabelki jest wszystko w pionie: wyniki-metoda
cisnienieTest=data.frame(wyniki,metoda)
cisnienieTest

bartlett.test(wyniki~metoda,cisnienieTest)

# p-value = 0.5009
# alfa < p-value -> brak podstaw do odrzucenia H0

# Interpretacja: Na poziomie istotności 0.05 nie mamy podstaw do odrzucenia 
# hipotezy o jednorodności wariancji.
# Wniosek: Możemy przeprowadzić ANOVE

# przez ANOVĘ testujemy średnią
# H0: ciśnienie nie ma wpływu na wielkość produkcji: mu1 = mu2 = mu3 = mu4
# H1: ~H0

n=length(wyniki) # liczba pomiarów
k=length(cisnienie[1,]) # liczba kategorii
model=lm(wyniki~metoda)

n
k
model

anova(model) # F-value = 2.2665

# Sposób 1:
# kwantyl rozkładu F Snedecorda z (n1-1,n2-1) stopniami swobody
qf(1-alpha,k-1,n-k) # 2,866
# F = 2,2665 < 2,866 -> brak podstaw do odrzucenia H0

# Sposób 2:
# p-value = 0,09735 > 0,05 = alpha -> brak podstaw do odrzucenia H0

# Interpretacja: Na poziomie istotności 0,05 dane potwierdzają hipotezę, że ciśnienie
# nie ma wpływu na wielkość produkcji.

# F > qf(...)   ->   odrzucamy H0
# F < qf(...)   -> brak podstaw do odrzucenia


# ZAD 2 check normalność

kopalnie=read.csv("Anova_kopalnie.csv",sep=";", dec=",")

alpha=0.01 # 

# testowanie jednorodności wariancji

# H0: wariancje są jednorodne
# H1: ~H0

metoda=rep(names(kopalnie),each=length(kopalnie$K1))
wyniki=c(kopalnie$K1,kopalnie$K2,kopalnie$K3,kopalnie$K4,kopalnie$K5)
kopalnieTest=data.frame(wyniki,metoda)
kopalnieTest

bartlett.test(wyniki~metoda,kopalnieTest)
# p-value = 0,03 > 0,01 = alpha -> brak podstaw do odrzucenia H0    #### tu nie może być alpha = 0,05, bo by były niejednorodne lol

# Interpretacja: Na poziomie istotności 0,01 dane potwierdzają jednorodność wariancji.
# Wniosek: Można zostać przeprowadzona ANOVA.

# H0: mu1 = mu2 = mu3 = mu4 = mu5
# H1: ~H0

model=lm(wyniki~metoda)
anova(model)

# Sposób 1:
n=length(wyniki) # liczba pomiarów
k=length(kopalnie[1,]) # liczba kategorii
qf(1-alpha,k-1,n-k) # 4,893
# F = 0,956 > 4,983 -> brak podstaw do odrzucenia H0

# Sposób 2:
# Pr(>F) = 0,4594 > 0,01 = alpha -> brak podstaw do odrzucenia H0

# Interpretacja: Na poziomie istotności 0,01 dane potwierdzają, że średnie zawartości popiołu
# dla ekogroszku produkowanego w pięciu kopalniach można uznać za jednakowe.


# ZAD 3 

mikrometr=read.csv("Anova_mikrometr.csv",sep=";", dec=",")

# założenie, że wariancje są takie same -> nie trzeba testować jendorodności wariancji

# H0: Wybór mikrometru nie ma wpływu na uzyskane wyniki: mu1 = mu2 = mu3
# H1: ~H0
# nie można zrobić, tak jak w pierwszym zadaniu, bo miejscami są puste komórki 
mikrometr
metoda=rep(names(mikrometr),c(length(na.omit(mikrometr$mikrometrI)),
                              length(na.omit(mikrometr$mikrometrII)),
                              length(na.omit(mikrometr$mikrometrIII))))
wyniki=c(na.omit(mikrometr$mikrometrI),
         na.omit(mikrometr$mikrometrII),
         na.omit(mikrometr$mikrometrIII))
metoda
wyniki

model=lm(wyniki~metoda)
anova(model)

alpha=0.05

# Sposób 1:
n=length(wyniki) # liczba pomiarów
k=length(mikrometr[1,]) # liczba kategorii
qf(1-alpha,k-1,n-k) # 3,885
# F = 3,378 < 3,885 -> brak podstaw do odrzucenia H0

# Sposób 2:
# Pr(>F) to to samo, co p-value
# p-value = 0,069 > 0,05 = alpha -> brak podstaw do odrzucenia H0

# Interpretacja: Na poziomie istotności 0,05 dane nie potwierdzają hipotezy, że wbór
# mikrometru ma wpływ na uzyskane wyniki.


# ZAD 4

sportowcy=read.csv("Anova_sportowcy.csv",sep=";")

metoda=rep(names(sportowcy),each=length(sportowcy$Niepalacy))
wyniki=c(sportowcy$Niepalacy,sportowcy$Lekkopalacy,sportowcy$Sredniopalacy,sportowcy$Duzopalacy)
sportowcyTest=data.frame(wyniki,metoda)

sportowcyTest

# a) 

alpha=0.01

# test jednorodności wariancji
# H0: wariancje są takie same
# H1: ~H0

bartlett.test(wyniki~metoda,sportowcyTest)
# p-value = 0,8517 > 0,01 = alpha -> brak podstaw do odrzucenia H0

# Interpetacja: Na poziomie istotności 0,01 dane potwierdzają jednorodność wariancji.
# Wniosek: Może zostać przeprowadzona ANOVA.

# H0: palenie papierosów nie ma wpływu na rytm zatokowy: mu1 = mu2 = mu3 = mu4
# H1: ~H0

model=lm(wyniki~metoda)
anova(model)

# Sposób 1:
n=length(wyniki) # liczba pomiarów
k=length(sportowcy[1,]) # liczba kategorii
qf(1-alpha,k-1,n-k) # 4,938
# F = 6,12 > 4,938 -> odrzucamy H0

# Sposób 2:
# Pr(>F) = 0,003979 < 0,01 = alpha -> odrzucamy H0

# Interpretacja: Na poziomie istotności 0,01 dane potwierdzają hipotezę, że palenie
# papierosów może wpływać na rytm zatokowy serca.

# b) test uczciwych różnic Tukeya (honest significant differences)

# alpha niepodane w zadaniu, więc przyjmuję alpha=0,05

TukeyHSD(aov(wyniki~metoda),ordered=TRUE)

# Jeżeli p adj < alpha to para (i,j) jest znacząco różna
# podobne: S-N, D-N, D-S, L-D

# grupy jednorodne to takie, które nie różnią się istotnie statystycznie między sobą

# dwie grupy jednorodne:
# 1: S-N-D (każdy z każdym jest podobne)
# 2: L-D

srednie=sapply(split(sportowcyTest$wyniki,sportowcyTest$metoda),mean)
srednie
plot(TukeyHSD(aov(wyniki~metoda),ordered=TRUE))


# ZAD 5 check normalność

chomiki=read.csv("Anova_chomiki.csv",sep=";")

metoda=rep(names(chomiki),c(length(na.omit(chomiki$I)),
                            length(na.omit(chomiki$II)),
                            length(na.omit(chomiki$III)),
                            length(na.omit(chomiki$IV))))
wyniki=c(na.omit(chomiki$I),
         na.omit(chomiki$II),
         na.omit(chomiki$III),
         na.omit(chomiki$IV))


# a) Czy słuszne jest przypuszczenie, że masa gruczołu tarczycowego zależy od poziomu inbredu?

alpha=0.05

# testowanie jednorodności wariancji
# H0: wariancje są takie same
# H1: ~H0

bartlett.test(wyniki~metoda,sportowcyTest)
# p-value = 0,8517 > 0,05 = alpha -> brak podstaw do odrzucenia H0

# Interpretacja: Na poziomie istotności 0,05 dane potwierdzają jednorodność wariancji.
# Wniosek: Może zostać przeprowadzona ANOVA.

# H0: Masa gruczołu tarczycowego nie zależy od poziomu inbredu: mu1 = mu2 = mu3 = mu4
# H1: ~H0

model=lm(wyniki~metoda)
anova(model)

# Sposób 1:
n=length(wyniki) # liczba pomiarów
n
k=length(chomiki[1,]) # liczba kategorii
k
qf(1-alpha,k-1,n-k) # 3,13
# F = 3,9515 > 3,13 -> odrzucamy H0

# Sposób 2:
# Pr(>F) = 0,024 < 0,05 -> odrzucamy H0

# Interpretacja: Na poziomie istotności 0,05 dane potwierdzają hipotezę, że masa
# gruczołu tarczycowego zależy od poziomu inbredu.

# b) test HSD Tukey'a

TukeyHSD(aov(wyniki~metoda),ordered=TRUE)

# podobne: II-I, III-I, III-II, IV-II, IV-III
# grupy jednorodne:
# 1: I-II-III
# 2: II-III-IV

# ZAD 6

pulapki=read.csv("C:/Users/OEM/OneDrive/Dokumenty/uniStuff/statystyka/laby/Anova_pulapki.csv",sep=";")

metoda=rep(names(pulapki),each=length(pulapki$rozsiany))
wyniki=c(pulapki$rozsiany,pulapki$skoncentrowany,pulapki$roslina.zywicielka,pulapki$powietrzny,pulapki$gruntowy)
pulapkiTest=data.frame(wyniki,metoda)

# a)

# testowanie jednorodności wariancji
# H0: wariancje są takie same
# H1: ~H0

alpha=0.05

bartlett.test(wyniki~metoda,pulapkiTest)
# p-value = 0,068 > 0,05 = alpha -> brak podstaw do odrzucenia H0

# Interpretacja: Na poziomie istotności 0,05 dane potwierdzają jednorodność wariancji.
# Wniosek: Może zostać przeprowadzona ANOVA.

# H0: Strategia lokalizacji nie ma wpływu na liczbę uwięzionych ciem cygańskich - średnie są takie same.
# H1: ~H0

model=lm(wyniki~metoda)
anova(model)

# Sposób 1:
n=length(wyniki) # liczba pomiarów
n
k=length(pulapki[1,]) # liczba kategorii
k
qf(1-alpha,k-1,n-k) # 2,87
# F = 39,382 > 2,87 -> odrzucamy H0

# Sposób 2:
# p-value ~ 0 < 0,05 = alpha -> odrzucamy H0

# Intepretacja: Na poziomie istotności 0,05 dane potwierdzają hipotezę, że strategia lokalizacji może
# mieć wpływ na liczbę uwięzionych ciem cygańskich.

# b) test HSD Tukey'a

TukeyHSD(aov(wyniki~metoda),ordered=TRUE)

# podobne: p-s, p-rż, s-rż; r-g
#grupy jednorodne:
# 1: p-s-rż
# 2: r-g
