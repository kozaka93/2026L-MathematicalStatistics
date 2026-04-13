#=======================================================#
#                  STATYSTYKA MATEMATYCZNA              # 
#                     LABORATORIUM 1                    #
#=======================================================#


# Cel: wybrane rozkłady prawdopodobieństwa --------------------------------

# norm - rozkład normalny
# chisq - rozkład chi-kwadrat
# gamma - rozkład gamma
# exp - rozkład wykładniczy
# f - rozkład F-Snedecora

# d - gęstość f(x) lub rozkład prawdopodobieństwa P(X = x)
# p - dystrybuanta F(x) = P(X <= x)
# q - funkcja kwantylowa 
# r - generowanie liczb pseudolosowych


# Zadanie 1 ---------------------------------------------------------------

# a) N(0, 1), N(1, 1), N(2, 1)


# gęstość



# dystrybuanta



# funkcja przeżycia 



# b) N(0, 1), N(0, 0.5), N(0, 2)


# gęstość



# dystrybuanta



# funkcja przeżycia



# Zadanie 2 ---------------------------------------------------------------

# a) 


# b)


# c)


# d)



# Zadanie 3 ---------------------------------------------------------------

# a)


# b)


# c)


# d)



# Zadanie 4 ---------------------------------------------------------------


# a)



# b)




# Zadanie 5 ---------------------------------------------------------------

# dist_func: nazwa funkcji (np. dgamma, dchisq, dt)
# params_list: lista list z parametrami dla każdej krzywej
# xlim: zakres osi X
# ylim: zakres osi Y (opcjonalny)
# main: tytuł wykresu
# labels: wektor nazw do legendy (opcjonalny)

plot_distributions <- function(dist_func, 
                               params_list, 
                               xlim, 
                               ylim = NULL, 
                               main,
                               labels = NULL) {
  
}

# --- Przykład użycia ---

params <- list(
  list(mean = 0, sd = 1),
  list(mean = 0, sd = 2),
  list(mean = 0, sd = 3)
)

plot_distributions(dnorm, 
                   params, 
                   xlim = c(-5, 5), 
                   main = "Porównanie gęstości rozkładu normalnego",
                   labels = c("Norm(0,1)", "Norm(0,2)", "Norm(0,3)"))


# Zadanie 6 ---------------------------------------------------------------



# Zadanie 7 ---------------------------------------------------------------



# Zadanie 8 ---------------------------------------------------------------


# a)



# b) 



# c) 



# Zadanie 9 ---------------------------------------------------------------



# Zadanie 10 --------------------------------------------------------------



# Zadanie 11 --------------------------------------------------------------


# a)



# b)



# Zadanie 12 --------------------------------------------------------------


# a)



# b)


