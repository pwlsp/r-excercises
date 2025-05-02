########################################################################################################
dane = read.csv("/home/pwlsp/room/put/statystyka/statystyka-repo/4-estymacja/dane_est.csv",sep=";",dec=",")

diamenty = na.omit(dane$diamenty)

n = length(diamenty)
n
# 12

# Populacja: wszystkie syntetyczne diamenty
# wyprodukowane nową metodą

# Próba: 12 syntet. diamentów wyprodukowanych nową metodą

# Badana zmienna : waga syntet. diamentów (w karatach)

srednia = mean(diamenty) # średnia próbkowa
srednia
# 0.5341667

### ---

# alfa = 1 - ufność
# alfa jest nazywana poziomem istotności

alfa = 1 - 0.95
odchylenie = sd(diamenty) # odchylenie standardowe z próby

Lz = srednia - qnorm(1-alfa/2) * odchylenie / sqrt(n)
Pz = srednia + qnorm(1-alfa/2) * odchylenie / sqrt(n)
Lt = srednia - qt(1-alfa/2, n-1) * odchylenie / sqrt(n)
Pt = srednia + qt(1-alfa/2, n-1) * odchylenie / sqrt(n)

Lz
Pz
Lt
Pt

########################################################################################################

ufnosc = function(srednia, odchylenie, sigma, n, alfa) {
  Lz = srednia - qnorm(1-alfa/2) * odchylenie / sqrt(n)
  Pz = srednia + qnorm(1-alfa/2) * odchylenie / sqrt(n)
  Lt = srednia - qt(1-alfa/2, n-1) * odchylenie / sqrt(n)
  Pt = srednia + qt(1-alfa/2, n-1) * odchylenie / sqrt(n)
  
  return(
    if (n<30) {
      if ( sigma==FALSE){
        print(paste("(",Lt,":",Pt,")"))}
      else {print(paste("(",Lz,":",Pz,")"))}
    }
    else{print(paste("(",Lz,":",Pz,")"))}
  )
}

ufnosc(srednia, odchylenie, FALSE, n, alfa)

# dolny kraniec przedziału ufności zaokrąglamy w dół, a górny w górę

# Na poziomie ufności 95% przedział (0.49, 0.57) pokrywa nieznaną porawdziwą
# średnią wagę syntetycznych diamentów wyprodukowanych nową metodą.

# można korzystać z t.test

t.test(diamenty, conf.level = 0.95)
t.test(diamenty, conf.level = 0.99)
t.test(diamenty, conf.level = 0.9)
# porównanie zmian zakresów (czy się zawęża czy rozszerza)
########################################################################################################

######## Zadanie 2 jako zadanie domowe

########################################################################################################
# Zadanie 3

papierosy = na.omit(dane$papierosy)

# odchylenie jest znane
sig = 0.7
alfa = 1 - 0.95
srednia = mean(papierosy)

ufnosc(srednia, sig, TRUE, 15, alfa)

# "( 1.45509096600647 : 2.1635757006602 )"
# przedział (1.45, 2.17) ZAOKRĄGLAMY NA ZEWNĄTRZ PRZEDZIAŁU ŻE TAK POWIEM



install.packages("BSDA")
library(BSDA)

z.test(papierosy, sigma.x = 0.7, conf.level = 0.95)

(2*qnorm(1-alfa/2) * 0.7 / 0.3) ^ 2 # 83.65844
# n >= (2*qnorm(1-alfa/2) * 0.7/0.3)^2

# odp. n = 84
