loty=read.csv("C:/Users/student/Desktop/piotrszyszka/loty.csv",sep=";")
class(oloty)

rok1959 = loty$X1959 #data.frame
rok1959

srednia = mean(rok1959)

#PISAĆ INTERPRETACJĘ (wyników?)

print(srednia)

sort(rok1959)

quantile(rok1959)

# Kwartyl pierwszy i trzeci nie liczy się tak jak medianę (czyli nie dzieli się na pół wartości środkowych)

Q1 = quantile(rok1959)[2] #kwantyl pierwszy jest pod "25%" czyli drugi w tabeli (indeks 2 z jakiegos powodu)
print(Q1)
# W roku 1959 w pierwszych trzech miesiacach liczba pasażerów linii lotniczej
# była mniejsza lub równa 388(7) osób
# a w pozostałych miesiącach liczba pasażerów
# była większa lub równa 388 osób

Q2 = quantile(rok1959)[3]
print(Q2)

# W pierwszych sześciu miesiącach liczba pasażerów była mniejsza niż 407 osób,
# a w pozostałych sześciu miesiącach liczba pasażerów była większ niż 407 osób.

Q3 = quantile(rok1959)[4]
print(Q3)
# W pierwszych sześciu miesiącach liczba pasażerów była mniejsza niż 465 osób,
# a w pozostałych sześciu miesiącach liczba pasażerów była większ niż 465 osób.

# Standard Deviation = sd
sd = sd(rok1959)
print(sd)
# Przeciętna liczba pasażerów w roku 1959
# pewnej linii lotniczej odchylała się od średniej
# o 70 osób

v = sd/srednia*100
print(v)
# Współczynnik zmienności wynosi około 16%,
# to znaczy, że liczba pasażerów w roku 1959 pewnej linii lotniczej
# jest słabo zróżnicowana (słabo to keyword, konkretna nazwa)

przedzialy = seq(200,650,length=7)
przedzialy
kolory=c("red", "yellow", "blue", "magenta", "green", "purple")

par(mfrow=c(3,2))
for(i in 1:6){
  hist(loty[,i], main=paste('loty w roku', names(loty)[i]), breaks=przedzialy, col = kolory)
}

min(loty)
max(loty)


par(mfrow=c(1,1))
boxplot(loty)
