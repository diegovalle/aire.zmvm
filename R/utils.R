is.integer2 <- function(int) {
  if (length(int) < 1)
    return(FALSE)
  if (any(is.na(int)))
    return(FALSE)
  tryCatch(identical(int, as.integer(floor(int))) |
             identical(int, as.double(floor(int))) |
             identical(int, as.single(floor(int))),
           error = function(e) {
             FALSE
             })
}

is.Date <- function(date, date.format = "%Y-%m-%d") {
  if (length(date) < 1)
    return(FALSE)
  tryCatch(!is.na(as.Date(date, date.format)),
           error = function(e) {
             FALSE
             })
}

# http://www.aire.cdmx.gob.mx/descargas/monitoreo/normatividad/NADF-009-AIRE-2006.pdf
# 4.20 Redondeo: Formato que modifica la información después de una cifra de interés, de tal manera que si la
# siguiente cifra es 4 ó menor no se considera y no cambia la cifra de interés. Cuando la cifra siguiente es 5 ó mayor,
# entonces la cifra de interés se incrementa en una unidad (17). Por ejemplo: el redondeo de la cifra 0.1105 es 0.111,
# y en el caso de la cifra 0.1104 el resultado es 0.110.
round_up <- function(x, n = 0) {
  posneg <-  sign(x)
  z <- abs(x) * 10 ^ n
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10 ^ n
  z * posneg
}
