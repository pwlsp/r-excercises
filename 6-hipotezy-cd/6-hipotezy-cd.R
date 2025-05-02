chomiki = read.csv("/home/pwlsp/room/put/statystyka/statystyka-repo/6-hipotezy-cd/Anova_chomiki.csv",sep=";",dec=",")
cisnienie = read.csv("/home/pwlsp/room/put/statystyka/statystyka-repo/6-hipotezy-cd/Anova_cisnienie.csv",sep=";",dec=",")

cisnienie

metoda = rep(names(cisnienie), each=length(cisnienie$Niskie))
wyniki = c(cisnienie$Niskie, cisnienie$Srednie, cisnienie$Silne, cisnienie$BardzoSilne)
metoda
wyniki
cisnienieTest = data.frame(wyniki, metoda)
cisnienieTest

srednie = sapply(split(cisnienieTest$wyniki, cisnienieTest$metoda), mean)
srednie

# H0: sig^2_1 = sig^2_2 = sig^2_3 = sig^2_4
# H1: ~H0

alfa = 0.05
bartlett.test(wyniki~metoda, cisnienieTest)
# p-value = 0.5009

# alfa < p-val
# Brak podstaw do odrzucenia H0.

# Na poziomie istotności 0.05 nie mamy podstaw do odrzucenia
# hipotezy o jednorodności wariancji.
# Możemy przeprowadzić ANOVE

# H0: mu1=mu2=mu3=mu4
# H1: ~H0

model = lm(wyniki~metoda)
anova(model)

# F = 2.2665

k = 4
n = 40

qf(1-alfa, k-1, n-k)
qf(0.95, 3, 36)

# qf(...) = 2.866266

# F > qf(...)   ->   odrzucamy H0
# F < qf(...)   -> brak podstaw do odrzucenia

########## coś tu jeszcze było
# II sposób

mikrometr = read.csv("/home/pwlsp/room/put/statystyka/statystyka-repo/6-hipotezy-cd/Anova_mikrometr.csv",sep=";",dec=",")
mikrometr

metoda = rep(names(mikrometr),
c(length(na.omit(mikrometr$mikrometrI)),
  length(na.omit(mikrometr$mikrometrII)),
  length(na.omit(mikrometr$mikrometrIII))
))

wyniki = c(na.omit(mikrometr$mikrometrI),
           na.omit(mikrometr$mikrometrII),
           na.omit(mikrometr$mikrometrIII)
)

metoda
wyniki

# zad 4
sportowcy = read.csv("/home/pwlsp/room/put/statystyka/statystyka-repo/6-hipotezy-cd/Anova_sportowcy.csv",sep=";",dec=",")

metoda = rep(names(sportowcy),
             each = length(sportowcy$Niepalacy))

wyniki = c(sportowcy$Niepalacy, sportowcy$Lekkopalacy, sportowcy$Sredniopalacy, sportowcy$Duzopalacy)
wyniki

sportowcyTest = data.frame(wyniki, metoda)

#H0 sig^2_1 = sig^2_2 = sig^2_3 = sig^2_4 
#H1: ~H0

model = lm(wyniki~metoda)
anova(model)

# w anova p-value wyszło mniejsze
# -> odrzucamy H0

# palenie papierosów wpływa na rytm zatokowy serca

# porównanie par średnich
# H0: mu_i = mu=j dla i = 1, 2, 3, 4
# H1: ~H0
# TUKEY TEST

TukeyHSD(aov(wyniki~metoda), ordered=TRUE)

# Jeżeli p adj < alfa
# to para (i,j) jest znacząco różna

# I grupa jednorodna {Ś,D,N}
# II grupa jednorodna {L,D}

