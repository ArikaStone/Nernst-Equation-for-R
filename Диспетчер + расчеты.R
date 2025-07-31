# диспетчер, который смотрит на NA и вызывает нужную функцию
compute_nernst <- function(ion, Cin = NA_real_, Cout = NA_real_, Ei = NA_real_, T = 310.15) {
  if (!ion %in% names(z_vals)) 
    stop("Неизвестный ион: ", ion)
  z <- z_vals[ion]
  
  # scenario 1: Ei NA, Cin и Cout есть
  if ( is.na(Ei) && !is.na(Cin) && !is.na(Cout) ) {
    return( nernst(Cout, Cin, z, T) )
  }
  # scenario 2: Ei есть, ровно одна из Cin/Cout NA
  if ( !is.na(Ei) && xor(is.na(Cin), is.na(Cout)) ) {
    return( nernst_conc(Ei, z, Cin, Cout, T) )
  }
  
  stop("Невозможно определить, что считать: проверьте, какие значения NA.")
}

# подготовим data.frame - ниже то, как загружала я, но ваш может отличаться
# ion_data <- read.csv("ion_data_with_z.csv", na.strings="NA", stringsAsFactors=FALSE)

ion_data$Result <- NA_real_

for (i in seq_len(nrow(ion_data))) {
  ion_i  <- ion_data$ion[i]
  Cin_i  <- ion_data$Cin[i]
  Cout_i <- ion_data$Cout[i]
  Ei_i   <- ion_data$Ei[i]
  
  ion_data$Result[i] <- compute_nernst(
    ion  = ion_i,
    Cin  = Cin_i,
    Cout = Cout_i,
    Ei   = Ei_i
  )
}

print(ion_data)
