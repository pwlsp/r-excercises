################################################################################
## zadanie 1

sin(2*pi)
#-2.449213e-16 traktujemy jak 0

cos(3/4)

tan(1/4*pi)

log10(100)
log(100, 10)

log(15)
# ln(15)

log(1/7, 7)
log(1/7, base = 7)
log(base = 7, 1/49) # zmiana kolejności CHYBA też działa

exp(1)
# e

exp(3)
# e^3

64^(1/3)
# pierwiastek 3 stopnia z 64

################################################################################
## zadanie 2

vector = seq(1, 10 , 1)
vector

sum(vector)

################################################################################
## zadanie 3

## a)

x = seq(2, 20, 2)
x

length(x)
# dlugosc 10

## b)

y = seq(20, 2, -2)
y

y2 = rev(x) # inny sposób
y2

## c)

x*x
x^2

# Zwracają to samo, czyli każda komórka jest kwadratem odpowiedniej komórki
# w oryginalnym wektorze x.

## d)

# Odległość euklidesowa to po prostu odległość między punktami tak jak w
# kartezjańskim układzie współrzędnych (przestrzeń euklidesowa to prawie
# to samo xd). Długość wektora analogicznie. Ten wektor jest 10-wymiarowy.

# Czyli no tak jak w 2 / 3 wymiarach - pitagoras,
# czyli do kwadratu, suma i pierwiastek.

dlugoscEuklidesowa = sqrt(sum(x^2))
dlugoscEuklidesowa


## e)
x
t(x)
t(t(x))

t(x)%*%y

x%*%t(y)

t(x)
t(t(y))

t(t(x))
t(y)

################################################################################
## zadanie 4

seq(5, 10, length.out = 13)

################################################################################
## zadanie 5

z1=rep(c(1,2),times=5)
z1
z2=rep(c(1,2),each=5)
z2

z1+4 #dodawanie wartości do każdego elementu

z3 = z2[-length(z2)]
z3

c=z1+z3 # błąd będzie, bo różnych długości

((1:10)*10)[c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)]

z1 > 1

z1[z1 > 1]

################################################################################
## zadanie 6

a = c(1, 3, 6, 2, 7, 4)
a

min(a)
which.min(a)
max(a)
which.max(a)

min(c(4, 2, NA, 6))
min(c(4, 2, NA, 6), na.rm = TRUE)

a[a<=4]

sum(a)

sum(a^2)

sqrt(sum(a^2))
length(a)

a[3]

b = a[-4]
b

################################################################################
## zadanie 7

w1 = c(2, 3, 0)
w2 = c(1, -1, 2)
w3 = c(1, 1, -1)

A = rbind(w1, w2, w3) # rows bind  | cbind do columns bind
A

A^2

A%*%A



t(A)
det(A)
solve(A)


e1 = c(1, 0, 0)
e2 = c(0, 1, 0)
e3 = c(0, 0, 1)
B = rbind(e1, e2, e3)
B
solve(A, B) #to samo co solve A

b = A[3,]
b

################################################################################
## zadanie 8

x = seq(5,17, length=10)
x
y = seq(2,8, length=10)
y

plot(x,y)
data.frame(x,y)
plot(data.frame(x,y))

rbind(x,y)
plot(rbind(x,y))

cbind(x,y)
plot(cbind(x,y))

################################################################################
## zadanie 9

y = function(x){x^2+3*x-5}
y

curve(y,-3,4) # curve(funkcja, od, do)

curve(sin,0,3.14)
curve(sin,0,pi)
curve(sin,-pi,2*pi)
