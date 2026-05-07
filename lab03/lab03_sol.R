#=======================================================#
#                  STATYSTYKA MATEMATYCZNA              # 
#                     LABORATORIUM 3                    #
#=======================================================#


# Lista 2 -----------------------------------------------------------------


# Zadanie 6 ---------------------------------------------------------------

proba_norm <- rnorm(200)

# histogram (prob = TRUE, aby skala y odpowiadała gęstości)
hist(proba_norm, prob = TRUE, 
     main = "Histogram, Estymator Jądrowy i Gęstość Teoretyczna",
     xlab = "Wartości", ylab = "Gęstość", 
     col = "lightgray", border = "white", ylim = c(0, 0.5))

# estymator jądrowy gęstości (kolor czerwony)
lines(density(proba_norm), col = "red", lwd = 2)

# gęstość teoretyczna rozkładu N(0,1) (linia przerywana, ciemnozielona)
curve(dnorm(x), add = TRUE, col = "darkgreen", lwd = 2, lty = 2)

# legenda
legend("topright", 
       legend = c("Estymator jądrowy", "Gęstość teoretyczna N(0,1)"),
       col = c("red", "darkgreen"), 
       lty = c(1, 2), 
       lwd = 2,
       bty = "n")

# Zadanie 7 ---------------------------------------------------------------

transakcje <- c(rnorm(80, 50, 10), rnorm(80, 200, 15))

# a)

srednia <- mean(transakcje)
mediana <- median(transakcje)

# b)

boxplot(transakcje, 
        main = "Wykres skrzynkowy kwot transakcji",
        ylab = "Kwota (zł)",
        col = "lightblue",
        horizontal = TRUE)

# c) 

gestosc <- density(transakcje)

plot(gestosc, 
     main = "Estymator jądrowy gęstości",
     xlab = "Kwota transakcji", 
     ylab = "Gęstość",
     lwd = 2, 
     col = "darkred")

# wypełnienie obszaru pod krzywą dla lepszej widoczności
polygon(gestosc, col = rgb(0.8, 0.2, 0.2, 0.3), border = "darkred")

# linia średniej i mediany dla porównania
abline(v = srednia, col = "blue", lwd = 2, lty = 2)
abline(v = mediana, col = "darkgreen", lwd = 2, lty = 3)

legend("topright", 
       legend = c("Gęstość", "Średnia", "Mediana"),
       col = c("darkred", "blue", "darkgreen"), 
       lty = c(1, 2, 3), 
       lwd = 2)

# Zadanie 8 ---------------------------------------------------------------

wzrost <- c(rnorm(100, 165, 5), rnorm(100, 180, 6))

# gęstość dla różnych parametrów adjust
d1 <- density(wzrost, adjust = 0.2)
d2 <- density(wzrost, adjust = 1.0)
d3 <- density(wzrost, adjust = 5.0)

# ustalamy limity osi Y na podstawie najwyższego wykresu (zazwyczaj ten najmniej wygładzony)
plot(d1, main = "Wpływ parametru 'adjust' na estymator jądrowy",
     xlab = "Wzrost (cm)", ylab = "Gęstość", 
     col = "red", lwd = 1, lty = 3)

lines(d2, col = "blue", lwd = 2)        # domyślne wygładzenie
lines(d3, col = "darkgreen", lwd = 2)   # silne wygładzenie


legend("topright", 
       legend = c("adjust = 0.2", 
                  "adjust = 1.0 (domyślny)", 
                  "adjust = 5.0"),
       col = c("red", "blue", "darkgreen"), 
       lty = c(3, 1, 1), 
       lwd = 2,
       bty = "n")


# Zadanie 9 ---------------------------------------------------------------

karaty <- diamonds$carat

# obliczenie gęstości dla różnych jąder
# używamy tej samej szerokości pasma (bw), aby porównanie było rzetelne
bw_val <- "nrd0" 

d_gauss <- density(karaty, kernel = "gaussian", bw = bw_val)
d_epan  <- density(karaty, kernel = "epanechnikov", bw = bw_val)
d_rect  <- density(karaty, kernel = "rectangular", bw = bw_val)
d_tri   <- density(karaty, kernel = "triangular", bw = bw_val)

# histogram
hist(karaty, breaks = 50, prob = TRUE, col = "gray90", border = "white",
     main = "Wpływ wyboru jądra na estymator gęstości",
     xlab = "Masa (karaty)", ylab = "Gęstość", ylim = c(0, 2))

# nakładanie krzywych
lines(d_gauss, col = "blue", lwd = 2)
lines(d_epan,  col = "red", lwd = 1.5)
lines(d_rect,  col = "darkgreen", lwd = 1.5)
lines(d_tri,   col = "purple", lwd = 1.5)

# legendy
legend("topright", 
       legend = c("Gaussian", "Epanechnikov", "Rectangular", "Triangular"),
       col = c("blue", "red", "darkgreen", "purple"), 
       lty = 1, lwd = 2, bty = "n")


# Lista 3 -----------------------------------------------------------------

# Cel: podstawy wnioskowania statystycznego; ------------------------------
# estymacja punktowa i przedziałowa ---------------------------------------


# Zadanie 1 ---------------------------------------------------------------

x1 <- rnorm(20)
x2 <- rnorm(100)
plot(ecdf(x1))
plot(ecdf(x2), add = TRUE, col = 2)
curve(pnorm(x), add = TRUE, col = 3, lw = 3)
legend("topleft", c("N(0,1) - 20","N(0,1) - 100", "N(0,1) - teoretyczna"), lty=1, col=1:3, text.col=1)


x3 <- rnorm(200)
plot(ecdf(x1))
plot(ecdf(x3), add = TRUE, col = 2)
curve(pnorm(x), add = TRUE, col = 3, lw = 3)
legend("topleft", c("N(0,1) - 20","N(0,1) - 200", "N(0,1) - teoretyczna"), lty=1, col=1:3, text.col=1)


x4 <- rnorm(1000)
plot(ecdf(x3))
plot(ecdf(x4), add = TRUE, col = 2)
curve(pnorm(x), add = TRUE, col = 3, lw = 3)
legend("topleft", c("N(0,1) - 220","N(0,1) - 1000", "N(0,1) - teoretyczna"), lty=1, col=1:3, text.col=1)


# Zadanie 2 ---------------------------------------------------------------

# a)

N <- 500
Y <- rcauchy(N)

med <- c()
sre <- c()
for(i in 1:N) {
  med[i] <- median(Y[1:i])
  sre[i] <- mean(Y[1:i])
}

plot(1:N, sre, type = "l", col = 1)
lines(1:N, med, type = "l", col = 2)
abline(h = 0, col = 3)
legend("topleft", c("mean","median", "a = 0"), lty = 1, col = 1:3, text.col = 1)

# b)

N <- 500
Y <- rcauchy(N)

st <- c()
r <- c()
for(i in 2:N){
  st[i-1] <- sd(Y[1:i])
  r[i-1] <- IQR(Y[1:i])/2
}

plot(2:N, st, type = "l", log = 'y', ylim = c(0.5, max(st)+1), col = 1)
lines(2:N, r, lty = 1, col = 2)
abline(h = 1, col = 3)
legend("topleft", c("sd","SQR", "b = 1"), lty = 1, col = 1:3, text.col = 1)


# Zadanie 3 ---------------------------------------------------------------

# a)

N <- 500
Y <- rnorm(N)

srednia <- cumsum(Y)/1:N
plot(1:N, srednia, type = "l")

med <- c()

for(i in 1:N){
  med[i] <- median(Y[1:i])
}

lines(1:N, med, lty = 1, col = 2)
abline(h = 0, col = 3)
legend("bottomright", c("mean", "median", "mu = 0"), lty = 1, col = 1:3)

# b)

N <- 500
Y <- rnorm(N)

st <- c()
r <- c()

for(i in 2:N){
  st[i-1] <- sd(Y[1:i])
  r[i-1] <- IQR(Y[1:i])/1.35
}

plot(2:N, st, type = "l", log = 'y', ylim = c(0.5, max(st)+1))
lines(2:N, r, lty = 1, col = 2)
abline(h = 1, col = 3)
legend("topleft", c("sd", "IQR/1.35", "sigma = 1"), lty = 1, col = 1:3)


