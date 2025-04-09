# DEPENDENCIES
install.packages("arm")
library(arm)


#oceny=read.csv("C:/Users/student/Desktop/piotrszyszka/oceny.csv",sep=";",dec=",")
oceny=read.csv("/home/pwlsp/room/put/statystyka/2-statystyka-opisowa/lab2-oceny.R",sep=";",dec=",")
class(oceny)

grupy=names(oceny)
print(oceny)


par(mfrow=c(2,2))

for (i in 1:4){
  title = paste("histogram liczebności", grupy[i])
  discrete.histogram(na.omit(oceny[,i]),freq=TRUE, main="title", xlab="oceny")
}

par(mfrow=c(1,1))
boxplot(oceny)


#GM1 = na.omit(oceny[,1])
#GM2 = na.omit(oceny[,2])
#GM3 = na.omit(oceny[,3])
#GM4 = na.omit(oceny[,4])
par(mfrow=c(1,4))
for (i in 1:4){
  gmi = table(na.omit(oceny[,i]))
  pie(gmi)
}
print(na.omit(oceny[,i]))
print(table(na.omit(oceny[,i])))