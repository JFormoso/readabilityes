#' @keywords internal
.onLoad <- function(libname, pkgname) {

  # -- Español: regex precompiladas -------------------
  .rx_valid_letters <<- sprintf("^[%s]+$", .valid_letters)
  .rx_vowel <<- "[aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00ef]"
  .rx_strong <<- "[a\u00e1e\u00e9o\u00f3]"
  .rx_weak <<- "[iu\u00fc\u00ef]"

  # -- Catalán: valid_letters sin duplicados ---------------------------------
  # .valid_letters_ca se define en constants.R concatenando strings;
  # aquí la normalizamos para que no haya caracteres repetidos.
  .valid_letters_ca <<- paste0(
    unique(strsplit(.valid_letters_ca, "", fixed = TRUE)[[1]]),
    collapse = ""
  )

  # -- Caché compartida --------------------------
  .syll_cache <<- new.env(parent = emptyenv())
}
