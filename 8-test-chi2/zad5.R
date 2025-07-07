# Define interval names and frequencies
intervals = c("(0; 2]", "(2; 4]", "(4; 6]", "(6; 8]", "(8; 10]")
frequencies = c(10, 25, 35, 20, 10)
p=seq(0,10,2) # -> 6 przedzialow
p
# Create a table object
frequency_table = structure(frequencies, names = intervals, class = "table")

# Check the class
class(frequency_table)

# View the table
print(frequency_table)

y = frequency_table

obserwacje=as.vector(y)
obserwacje
oczekiwane=c()

for(i in 1:k) {
    oczekiwane=c(oczekiwane, pnorm(p[i+1], m, s) - pnorm(p[i],m,s))
}
oczekiwane
sum(oczekiwane) # 0.9932772 < 1 
# przerabianie tak, aby suma całego prawdopodobieństwa wyszła 1
oczekiwane[1]=pnorm(p[2],m,s)
oczekiwane[k]=1-pnorm(p[k],m,s)

sum(oczekiwane) # 1

# mniej niż 5 pomiarów -> sie sumuje

oczekiwane[k-1]=oczekiwane[k-1]+oczekiwane[k]
oczekiwane=oczekiwane[-k] # usuwanie ostatniej kolumny

# same thing with obserwacje
obserwacje[k-1]=obserwacje[k-1]+obserwacje[k]
obserwacje=obserwacje[-k]

chisq.test(obserwacje, p=oczekiwane)
# p-value = 0.92 > 0.05 = alfa -> brak podstaw do odrzucenia H0

# Interpretacja: Na poziomie istotności 0.05 stężenie ozonu ma rozkład normalny.


