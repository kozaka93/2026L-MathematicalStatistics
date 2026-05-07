#=======================================================#
#                  STATYSTYKA MATEMATYCZNA              # 
#                     LABORATORIUM 4                    #
#=======================================================#


# Lista 3 -----------------------------------------------------------------


# Zadanie 4 ---------------------------------------------------------------

n <- 10000
theta <- 1

theta_M <- c()
theta_NW <- c()

for (i in 1:n){
  proba <- runif(20, 0, theta)
  theta_M[i] <- 2*mean(proba)
  theta_NW[i] <- max(proba)
}

b_M <- mean(theta_M) - theta
# -0.0005474654 - niedoszacowujemy
b_NW <- mean(theta_NW) - theta
# -0.04750139 - niedoszacowujemy

mse_M <- var(theta_M) + b_M^2
# 0.01638344

mse_NW <- var(theta_NW) + b_NW^2
# 0.004260156


# Zadanie 5 ---------------------------------------------------------------

N <- 10000
n <- 10
mi <- 9 
sigma <- 3
alfa <- 0.05
q <- qt(1 - alfa/2, n - 1)
k <- 0
for(i in 1:N){
  x <- rnorm(n, mi, sigma)
  m <- mean(x)
  s <- sd(x)
  if((mi >= m - q*s/sqrt(n)) & (mi <= m + q*s/sqrt(n))) k <- k +1
}

k/N


# ilustracja

N <- 100

przedzial <- replicate(N, {
  x <- rnorm(n) 
  sr <- mean(x)
  odch <- sd(x)
  c(sr - q * odch / sqrt(n), sr + q * odch / sqrt(n))
})
lewy <- przedzial[1, ]
prawy <- przedzial[2, ]

matplot(
  rbind(lewy, prawy),
  rbind(1:N, 1:N),
  type = "l",
  xlab = "",
  ylab = "",
  col = "red",
  lty = 1
)
abline(v = 0)


# Zadanie 6 ---------------------------------------------------------------

n <- 50
m <- 48.4
sigma <- 8.75
alfa <- 0.05
q <- qnorm(1 - alfa/2)

c(m - q*sigma/sqrt(n), m + q*sigma/sqrt(n))



# Zadanie 7 ---------------------------------------------------------------

temp <- scan(nlines = 3)
330.0 322.0 345.0 328.6 331.0 342.0
342.4 340.4 329.7 334.0 326.5 325.8
337.5 327.3 322.6 341.0 340.0 333.0

prz.ufn.mi <- function(x, alfa) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  q <- qt(1 - alfa / 2, n - 1)
  prz <- m + c(-1, 1) * s * q / sqrt(n)
  prz
}

prz.ufn.mi(temp, 0.05)

t.test(temp, conf.level = 0.95)$conf


prz.ufn.war <- function(x, alfa) {
  n <- length(x)
  q1 <- qchisq(1 - alfa / 2, n - 1)
  q2 <- qchisq(alfa / 2, n - 1)
  prz <- (n - 1) * var(x) / c(q1, q2)
  list(wariancja = prz, odchylenie = sqrt(prz))
}

prz.ufn.war(temp, 0.05)


# Zadanie 8 ---------------------------------------------------------------

k <- 578
n <- 1014

p_hat <- k / n
alpha <- 0.05
q <- qnorm(1 - alpha / 2)

p_hat + c(-1, 1) * q * sqrt(p_hat * (1 - p_hat) / n)

# dokładne
binom.test(k, n, conf.level = 0.95)$conf


prop.test(k, n, conf.level = 0.95)$conf.int

# Zadanie 9 ---------------------------------------------------------------

binom.test(3, 12, conf.level = 0.95)$conf

