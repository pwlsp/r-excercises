# DEPENDENCIES
install.packages("arm")
library(arm)


#oceny=read.csv("C:/Users/student/Desktop/piotrszyszka/oceny.csv",sep=";",dec=",")
oceny=read.csv("/home/pwlsp/room/put/statystyka/statystyka-repo/2-statystyka-opisowa/oceny.csv",sep=";",dec=",")
class(oceny)

grupy=names(oceny)
print(grupy)
print(oceny)

m1 = oceny$grupa.M1
m = mean(na.omit(m1))
m
quantile(m1, na.rm = TRUE)
quantile(na.omit(m1))
quantile(na.omit(m1))[3]
quantile(na.omit(m1))[2]
quantile(na.omit(m1))[4]
sd = sd(na.omit(m1))
sd
v = sd / m * 100
v
summary(m1)

par(mfrow=c(2,2))

for (i in 1:4){
  title = paste("histogram liczebności", grupy[i])
  discrete.histogram(na.omit(oceny[,i]),freq=TRUE, main=grupy[i], xlab="oceny")
} # count (liczebnosc)

for (i in 1:4){
  title = paste("histogram liczebności", grupy[i])
  discrete.histogram(na.omit(oceny[,i]),freq=FALSE, main=grupy[i], xlab="oceny")
} # probability

par(mfrow=c(1,1))
boxplot(oceny)


#GM1 = na.omit(oceny[,1])
#GM2 = na.omit(oceny[,2])
#GM3 = na.omit(oceny[,3])
#GM4 = na.omit(oceny[,4])
table(na.omit(oceny))
par(mfrow=c(1,4))
for (i in 1:4){
  print(table(na.omit(oceny[,i])))
  gmi = table(na.omit(oceny[,i]))
  pie(gmi)
}

oceny$grupa.M1
elo = cut(na.omit(oceny[,1]), 3)
print(elo)
table(elo)
pie(table(elo))
# szereg rozdzielczy przedziałowy
  
print(na.omit(oceny[,1]))
print(table(na.omit(oceny[,1])))

