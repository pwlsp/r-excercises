install.packages("TeachingDemos")
library(TeachingDemos)

fizjo = c(8, 12, 26, 10, 23, 21, 16, 22, 18, 17, 36, 9)

ttt = t.test(fizjo, conf.level=0.95)
ttt
ttt$conf.int # confidence interval

sigma.test(fizjo)

# sqrt(sigma.test(fizjo))
# z ufnością 95% przedział od 5h44min do 13h47min pokrywa prawdziwe
# odchylenie standardowe czasu korzystania z urządzenia przez WSZYSTKICH pacjentów