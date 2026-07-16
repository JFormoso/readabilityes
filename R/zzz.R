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

  # -- Registro central por idioma ---------
  .lang_registry <<- list(
    es = list(
      label         = "Espa\u00f1ol",
      valid_letters = .valid_letters,
      v_fuerte      = .v_fuerte,
      v_debil       = .v_debil,
      v_tilde       = .v_tilde,
      clusters      = .clusters_lr,
      syllabify_fn  = .syllabify_word_es
    ),
    ca = list(
      label         = "Catal\u00e0",
      valid_letters = .valid_letters_ca,
      v_fuerte      = .v_fuerte_ca,
      v_debil       = .v_debil_ca,
      v_tilde       = .v_tilde_ca,
      clusters      = .clusters_ca,
      syllabify_fn  = .syllabify_word_ca
    )
  )
}
