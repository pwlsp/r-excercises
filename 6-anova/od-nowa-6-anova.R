cisnienie = read.csv("Anova_cisnienie.csv",sep=";",dec=",")

### Zadanie 3

mikrometr = read.csv("Anova_mikrometr.csv",sep=";", dec=",")
mikrometr

length(na.omit(mikrometr)$mikrometrI)
obiekty = rep(names(mikrometr),
              c(length(na.omit(mikrometr$mikrometrI)),
                length(na.omit(mikrometr$mikrometrII)),
                length(na.omit(mikrometr$mikrometrIII))))
obiekty

wyniki = c(na.omit(mikrometr$mikrometrI), na.omit(mikrometr$mikrometrII), na.omit(mikrometr$mikrometrIII))
wyniki
length(wyniki)

mikrometrTest = data.frame(wyniki, obiekty)
mikrometrTest

# tu akurat nie trzeba bartlett.testu

model = lm(wyniki~obiekty)
model
anova(model)
# p-value = 0.069 > alfa, a zatem brak podstaw do odrzucenia H0: mu mu mu mu



##### ZADANIE 5

chomiki = read.csv("Anova_chomiki.csv",sep=";")
chomiki

obiekty = rep(names(chomiki),
              c(length(na.omit(chomiki$I)),
                length(na.omit(chomiki$II)),
                length(na.omit(chomiki$III)),
                length(na.omit(chomiki$IV))))
obiekty

wyniki = c(na.omit(chomiki$I), na.omit(chomiki$II), na.omit(chomiki$III), na.omit(chomiki$IV))
wyniki

chomikiTest = data.frame(wyniki, obiekty)
chomikiTest

# Analiza wariancji
# H0: sig1^2=sig2^2=sig3^2=sig4^2=sig5^2
# H1: ~H0

bartlett.test(wyniki~obiekty, chomikiTest)
# p-value = 0.2   >   alfa = 0.05
# zatem brak podstaw do odrzucenia H0
# brak podstaw do odrzucenia hipotezy o jednorodności wariancji

# Można przeprowadzić ANOVA

# H0: mu1 =mu2 =mu3 =mu4 =mu5 (strategia lokalizacji nie ma wpływu na liczbę uwięzionych ciem)
# H1: ~H0

model = lm(wyniki~obiekty)
anova(model)
# p-value = 0.02398   <   alfa = 0.05
# a zatem odrzucamy H0
# czyli na poziomie istotności 0.05 poziom inbredu ma wpływ na masę gruczołu tarczycowego

TukeyHSD(aov(wyniki~obiekty), ordered = TRUE)

# podobne: II-I, III-I, III-II, IV-II, IV-III
# grupy jednorodne: I-II-III, II-III-IV






#### ZADANIE 6

pulapki = read.csv("Anova_pulapki.csv", sep=";", dec=",")
pulapki

obiekty = rep(names(pulapki), each=length(pulapki$rozsiany))
obiekty

wyniki = c(pulapki$rozsiany, pulapki$skoncentrowany, pulapki$roslina.zywicielka, pulapki$powietrzny, pulapki$gruntowy)
wyniki

pulapkiTest = data.frame(wyniki, obiekty)
pulapkiTest

# Analiza wariancji
# H0: sig1^2=sig2^2=sig3^2=sig4^2=sig5^2
# H1: ~H0

bartlett.test(wyniki~obiekty, pulapkiTest)
# p-value = 0.06804   >   alfa = 0.05
# zatem brak podstaw do odrzucenia H0
# brak podstaw do odrzucenia hipotezy o jednorodności wariancji

# Można przeprowadzić ANOVA

# H0: mu1 =mu2 =mu3 =mu4 =mu5 (strategia lokalizacji nie ma wpływu na liczbę uwięzionych ciem)
# H1: ~H0

model = lm(wyniki~obiekty)
anova(model)
# p-value = 3.252e-09   <   alfa = 0.05
# a zatem odrzucamy H0
# czyli na poziomie istotności 0.05 metoda ma wpływ na liczbę uwięzionych ciem

TukeyHSD(aov(wyniki~obiekty), ordered = TRUE)

# podobne: r-g, s-z, p-z, p-s
# grupy jednorodne: p-z-s, r-g
