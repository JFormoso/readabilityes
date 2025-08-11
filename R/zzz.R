#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Construir regex
  .rx_valid_letters <<- sprintf("^[%s]+$", .valid_letters)
  .rx_vowel         <<- "[aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00ef]"
  .rx_strong        <<- "[a\u00e1e\u00e9o\u00f3]"
  .rx_weak          <<- "[iu\u00fc\u00ef]"

  # Crear caché
  .syll_cache <<- new.env(parent = emptyenv())
}


