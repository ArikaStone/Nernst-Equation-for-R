nernst <- function(Cout, Cin, z, T = 310.15) {
  R <- 8.314
  F <- 96485
  coef <- (R * T) / (z * F) * 1000
  coef * log(Cout / Cin)
}