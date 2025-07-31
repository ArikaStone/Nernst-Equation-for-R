# температура задана из расчета 37°C, меняйте в зависимости от ваших условий
# наименование z обозначает валентность иона, ее нужно указывать в вашем data.frame
nernst_conc <- function(Ei, z, Cin = NA_real_, Cout = NA_real_, T = 310.15) {
  R <- 8.314
  F <- 96485
  coef <- (R * T) / (z * F) * 1000
  
  # если заданы обе концентрации или ни одной — ругаемся
  if ( !is.na(Cin) && !is.na(Cout) ) {
    stop("Укажите только Cin или только Cout для расчёта концентрации")
  }
  if ( is.na(Cin) && is.na(Cout) ) {
    stop("Нужно указать Cin или Cout")
  }
  
  # если надо найти внешнюю Cout
  if ( is.na(Cout) ) {
    return( Cin * exp(Ei / coef) )
  }
  # иначе — найти внутреннюю Cin
  return( Cout * exp(-Ei / coef) )
}
nernst_conc <- function(Ei, z, Cin = NA_real_, Cout = NA_real_, T = 310.15) {
  R <- 8.314
  F <- 96485
  coef <- (R * T) / (z * F) * 1000
  
  # если заданы обе концентрации или ни одной — ругаемся
  if ( !is.na(Cin) && !is.na(Cout) ) {
    stop("Укажите только Cin или только Cout для расчёта концентрации")
  }
  if ( is.na(Cin) && is.na(Cout) ) {
    stop("Нужно указать Cin или Cout")
  }
  
  # если надо найти внешнюю Cout
  if ( is.na(Cout) ) {
    return( Cin * exp(Ei / coef) )
  }
  # иначе — найти внутреннюю Cin
  return( Cout * exp(-Ei / coef) )
}
