#=======================================================#
#                  STATYSTYKA MATEMATYCZNA              # 
#                     LABORATORIUM 2                    #
#=======================================================#

# Cel: statystyka opisowa -------------------------------------------------

library(dplyr)
library(moments)


# Lista 1 -----------------------------------------------------------------

# Zadanie 12 --------------------------------------------------------------

# a)  ---------------------------------------------------------------------

# metoda Monte Carlo
n <- 20000
x <- runif(n)
y <- runif(n)
plot(x, y, pch = ".") # punkty w kwadracie
curve(x^2, col = 2, add = TRUE, lwd = 3) # funkcja z zadania

# tworzymy wektor z, który zawiera informację czy punkt jest pod krzywą czy nad
z <- y < x*x

# obliczamy pole
mean(z)

points(x[z], y[z], col = 2)

# b)  ---------------------------------------------------------------------

n <- 20000
x <- runif(n, -1, 1)
y <- runif(n)
plot(x, y, pch = ".")
curve(x^2, col = 2, add = TRUE, lwd = 3)
curve(1-x^2, col = 2, add = TRUE, lwd = 3)

z <-  x * x < y & y < 1 - x * x

points(x[z], y[z], col = 2)

mean(z)*2 # mnożymy razy 2, ponieważ pole obszaru w którym losujemy punkty wynosi 2


# Lista 2 -----------------------------------------------------------------

# Zadanie 1 ---------------------------------------------------------------

nauka <- c(
  20, 35, 45, 50, 55, 60, 60, 65,
  70, 75, 80, 80, 85, 90, 95, 
  seq(from = 100, to = 140, by = 10),
  150, 150, 
  seq(from = 160, to = 330, by = 10)
)

# a) szereg rozdzielczy przedziałowy

# funkcja cut
?cut 
przedzialy <- cut(nauka, breaks = 6)
tabela <- as.data.frame(table(przedzialy))


# b) liczności skumulowane i częstości względne

tabela$Liczność_skumulowana <- cumsum(tabela$Freq)
tabela$Częstość_względna <- prop.table(tabela$Freq)
# lub 
tabela$Częstość_względna <- tabela$Freq / length(nauka)

# c) histogram

hist(nauka, 
     breaks = 6, 
     main = "Histogram czasu nauki", 
     col = "lightblue",
     xlab = "Czas nauki [min]")


# Zadanie 2 ---------------------------------------------------------------

# a) 

hist(diamonds$carat)
hist(diamonds$carat, breaks = 6)
hist(diamonds$carat, breaks = 14)
hist(diamonds$carat, breaks = "FD")

# b)

h <- hist(diamonds$carat, breaks = 14)
lines(h$mids, h$counts, col = 3, lwd = 3)

# c)

set.seed(2204)
diamonds_small <- diamonds[sample(1:nrow(diamonds), 50), ]

stem(diamonds_small$carat)

# d)
var(diamonds$carat)
sd(diamonds$carat)
IQR(diamonds$carat)
diff(range(diamonds$carat))

summary(diamonds$carat)


# e)
boxplot(diamonds$price)

# f)

quantile(diamonds$carat, 0.05)
quantile(diamonds$carat, 0.1)
quantile(diamonds$carat, 0.25)
quantile(diamonds$carat, 0.5)
quantile(diamonds$carat, 0.75)
quantile(diamonds$carat, 0.9)
quantile(diamonds$carat, 0.95)

# lub

quantile(diamonds$price, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))

# g)

prices <- diamonds$price

# porównanie średniej uciętej ze średnią arytmetyczną i medianą 
prices <- diamonds$price

srednia <- mean(prices)
mediana <- median(prices)
srednia_ucieta_10 <- mean(prices, trim = 0.1)

# histogram
hist(prices, breaks = 100, col = "lightgray", border = "white",
     main = "Rozkład cen diamentów", xlab = "Cena (USD)", ylab = "Częstość")

# linie
abline(v = srednia, col = "blue", lwd = 2)             # średnia arytmetyczna
abline(v = mediana, col = "red", lwd = 2, lty = 2)     # mediana (linia przerywana)
abline(v = srednia_ucieta_10, col = "darkgreen", lwd = 2) # średnia ucięta 10%

# legenda
legend("topright", legend = c("Średnia", "Mediana", "Średnia Ucięta 10%"),
       col = c("blue", "red", "darkgreen"), lty = c(1, 2, 1), lwd = 2)


# zmiana wartości średniej wraz ze zmianą stopnia ucięcia

# poziomy ucięcia od 0% do 45%
stopnie_uciecia <- seq(0, 0.45, by = 0.01)

# średnia dla każdego poziomu
wartosci_sredniej <- sapply(stopnie_uciecia, function(x) mean(prices, trim = x))

# wykres liniowy
plot(stopnie_uciecia, wartosci_sredniej, type = "l", col = "darkred", lwd = 2,
     main = "Wpływ ucięcia na wartość średniej ceny",
     xlab = "Stopień ucięcia (proporcja)", ylab = "Wartość średniej")

# linia mediany dla porównania (ucięcie 50% to mediana)
abline(h = mediana, col = "blue", lty = 3)
text(0.1, mediana + 50, "Poziom mediany", col = "blue")

# a)
sleep <- msleep$sleep_total

IQR(sleep, na.rm = TRUE)
sd(sleep, na.rm = TRUE)

# b)
q1 <- quantile(sleep, 0.25)
q3 <- quantile(sleep, 0.75)
iqr_val <- IQR(sleep)

dolna_granica <- q1 - 1.5 * iqr_val
górna_granica <- q3 + 1.5 * iqr_val

# identyfikacja wartości odstających
outliery <- sleep[sleep < dolna_granica | sleep > górna_granica]

# c)
sleep_herbi <- msleep$sleep_total[msleep$vore == "herbi"]
sleep_carni <- msleep$sleep_total[msleep$vore == "carni"]

sd_herbi <- sd(sleep_herbi, na.rm = TRUE)
sd_carni <- sd(sleep_carni, na.rm = TRUE)

# Zadanie 4 ---------------------------------------------------------------

boxplot(sleep_total ~ vore, 
        data = msleep, 
        main = "Czas snu w zależności od rodzaju pożywienia",
        xlab = "Rodzaj pożywienia (vore)",
        ylab = "Całkowity czas snu (h)")

# Zadanie 5 ---------------------------------------------------------------

set.seed(2204)
dane_wykladnicze <- rexp(500, rate = 0.2)

# a)

hist(dane, breaks = 30, col = "skyblue", border = "white",
     main = "Histogram rozkładu wykładniczego (lambda = 0.2)",
     xlab = "Wartość", ylab = "Częstość")


# b) i c)

# przykład z wykorzystaniem funkcji 
oblicz_statystyki <- function(x) {
  n <- length(x)
  s <- sd(x)
  srednia <- mean(x)
  
  # skośność 
  skosnosc <- (sum((x - srednia)^3) / n) / (s^3)
  
  # kurtoza (K)
  kurtoza <- (sum((x - srednia)^4) / n) / (s^4) - 3
  
  return(list(skosnosc = skosnosc, kurtoza = kurtoza))
}

wyniki <- oblicz_statystyki(dane)

cat("Współczynnik skośności (g1):", wyniki$skosnosc, "\n")
cat("Współczynnik kurtozy (K):", wyniki$kurtoza, "\n")


# pakiet moments
skewness(dane) 
kurtosis(dane) # wzór w pakiecie moments na kurtozę jest bez -3  