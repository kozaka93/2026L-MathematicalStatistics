#=======================================================#
#                  STATYSTYKA MATEMATYCZNA              # 
#                     LABORATORIUM 5                    #
#=======================================================#


# Lista 3 -----------------------------------------------------------------


# Zadanie 10 --------------------------------------------------------------

x <- chickwts$weight[chickwts$feed == "soybean"]

# Funkcja z Zadania 7 (lista 3, lab04)
prz.ufn.war <- function(x, alfa) {
  n <- length(x)
  q1 <- qchisq(1 - alfa / 2, n - 1)
  q2 <- qchisq(alfa / 2, n - 1)
  prz <- (n - 1) * var(x) / c(q1, q2)
  list(wariancja = prz, odchylenie = sqrt(prz))
}

prz.ufn.war(x, 0.07)


# Zadanie 11 --------------------------------------------------------------

faithful


# I sposób - funkcja t.test -----------------------------------------------

t.test(faithful$waiting, conf.level = 0.99)$conf


# II sposób - funkcja Zadanie 7, lista 3 ----------------------------------

prz.ufn.mi <- function(x, alfa) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  q <- qt(1 - alfa / 2, n - 1)
  prz <- m + c(-1, 1) * s * q / sqrt(n)
  prz
}

prz.ufn.mi(faithful$waiting, 0.01)

# Zadanie 12 --------------------------------------------------------------

library(MASS)

k <- length(Pima.te$age[Pima.te$type == "Yes"])
n <- length(Pima.te$age)

binom.test(k, n, conf.level = 0.95)$conf

# wiek >= 35 --------------------------------------------------------------

k <- length(Pima.te$age[Pima.te$type == "Yes" & Pima.te$age >= 35])
n <- length(Pima.te$age[Pima.te$age >= 35])

binom.test(k, n, conf.level = 0.95)$conf
