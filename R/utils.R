is.integer2 <- function(int) {
  if (length(int) < 1)
    return(FALSE)
  if(any(is.na(int)))
    return(FALSE)
  tryCatch(identical(int, as.integer(floor(int))) |
             identical(int, as.double(floor(int))) |
             identical(int, as.single(floor(int))),
           error = function(e) {FALSE})
}

is.Date <- function(date, date.format = "%Y-%m-%d") {
  if (length(date) < 1)
    return(FALSE)
  tryCatch(!is.na(as.Date(date, date.format)),
           error = function(e) {FALSE})
}
