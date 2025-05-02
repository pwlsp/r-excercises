#kalkulator
a=2+2
print(a)

#Zadanie1

#-2.449213e-16 traktujemy jak 0
sin(2*pi)
cos(3/4)
tan(pi)

#można z base=podstawa, ale można też po prostu po przecinku (somyślnie base=exp(1))
log(100,10)
log(15)
log(1/7,7)

exp(3)
64^(1/3)

#Zadanie2
vector = seq(1,10,1)
vector
sum(vector)

#Zadanie3
x=seq(2,20,2)
length(x)
y=rev(x) #odwracanie wektora / inny sposób -> y=seq(20,2,-2)
x*x
x^2
x
sqrt(sum(x^2)) #odległość euklidesowa
t(x)%*%y #wynikiem jest liczba
x%*%t(y) #wynikiem jest macierz

#Zadanie4
seq(5,10,length=13)

#Zadanie5
z1=rep(c(1,2),times=5)
z2=rep(c(1,2),each=5)

z1+4 #dodawanie wartości do każdego elementu

z3=z2[-length(z2)]

c=z1+z3 #niepoprawny wynik dodawania

z4=z1[z1>1]
  
#Zadanie6
a=c(1,3,6,2,7,4)
a[which.min(a)] #wartość najmniejsza
which.min(a) #indeks najmniejszej wartości
a[a<=4] #scam, to nie indeksy

sum(a)
sum(a^2)
length(a)

a[3]
b=a[-(4)] #wektor a bez czwartej składowej

#Zadanie7
w1=c(2,3,0)
w2=c(1,-1,2)
w3=c(1,1,-1)
A=rbind(w1,w2,w3)

A^2
A%*%A

t(A) #transformacja
det(A) #wyznacznik
solve(A) #macierz odwrotna

b=A[3,] #tylko trzeci wiersz macierzy

#Zadanie8
x=seq(4,22,length=10)
y=seq(7,20,length=10)

plot(x,y)

plot(data.frame(x,y))

plot(rbind(x,y))

plot(cbind(x,y))

#Zadanie9
y=function(x){x^2+3*x+5}
curve(y,-3,4) #wykresy funkcji zmiennych losowych typu ciągłego

y=function(x){0.01^x}
curve(y, 0, 0.8)
