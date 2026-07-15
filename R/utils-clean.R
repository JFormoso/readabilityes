#' Limpia y normaliza una palabra
#'
#' Convierte a minúsculas, recorta espacios y elimina cualquier carácter
#' que no pertenezca al conjunto de letras válidas del idioma indicado.
#' Si tras limpiar no queda nada, devuelve `NA`.
#'
#' @param word Cadena de texto (longitud 1). Si es `NA` o no es `character`,
#'   devuelve `NA`.
#' @param lang Código de idioma: `"es"` (por defecto) o `"ca"`. Determina
#'   qué letras se consideran válidas.
#' @return Cadena limpia o `NA_character_` si queda vacía.
#' @examples
#' clean_word("  Canci\u00f3n!  ")        # "canción"
#' clean_word("l'Anna", lang = "ca")  # "l'anna"  (apóstrofe se preserva)
#' clean_word("pa\u00efsos", lang = "ca") # "països"
#' @export
#' @family syllabify
#' @seealso syllabify-internals
clean_word <- function(word, lang = syll_get_option("lang")) {
  if (length(word) != 1L || !is.character(word) || is.na(word)) {
    return(NA_character_)
  }
  w <- tolower(trimws(word))

  # Seleccionar letras válidas según idioma
  valid <- switch(lang,
                  "ca" = .valid_letters_ca,
                  "es" = .valid_letters,
                  stop(sprintf("Idioma no soportado en clean_word(): '%s'.", lang), call. = FALSE)
  )

  # En catalán preservar apóstrofe (recto y tipográfico) para que
  # .preprocess_word_ca() pueda operar sobre él después
  if (lang == "ca") {
    w <- gsub("\u2019", "'", w, fixed = TRUE)
    w <- gsub(sprintf("[^%s'\\-]", valid), "", w, perl = TRUE)
  } else {
    w <- gsub(sprintf("[^%s]", valid), "", w, perl = TRUE)
  }

  if (identical(w, "")) NA_character_ else w
}

#' ¿La palabra contiene solo letras válidas?
#'
#' Verifica que todos los caracteres pertenezcan al alfabeto interno soportado
#' para el idioma indicado.
#'
#' @param word Cadena de texto (longitud 1).
#' @param lang Código de idioma: `"es"` o `"ca"`.
#' @return `TRUE`/`FALSE` o `NA` si `word` es inválida.
#' @examples
#' is_valid_word("pingüino")             # TRUE
#' is_valid_word("pa\u00efsos", lang = "ca")  # TRUE
#' is_valid_word("hola!")                # FALSE
#' @export
#' @family syllabify
is_valid_word <- function(word, lang = syll_get_option("lang")) {
  w <- clean_word(word, lang = lang)
  if (is.na(w)) return(NA)
  if (grepl("'", w, fixed = TRUE)) return(FALSE)
  identical(w, tolower(trimws(word)))
}

#' Tokeniza y limpia un texto en palabras
#'
#' Divide un texto por separadores simples (espacios, guiones y puntuación),
#' limpia cada token y descarta vacíos.
#'
#' @param text Cadena de texto (puede contener varias palabras).
#' @param lang Código de idioma: `"es"` o `"ca"`.
#' @return Vector de palabras limpias (puede ser de longitud 0).
#' @examples
#' tokenize_clean("\u00a1Hola, mundo\u2014cruel!")        # c("hola","mundo","cruel")
#' tokenize_clean("l'Anna va a l'escola", lang = "ca")  # conserva apóstrofes
#' @export
#' @family syllabify
tokenize_clean <- function(text, lang = syll_get_option("lang")) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) return(character())
  raw <- unlist(
    strsplit(text, "[^\\p{L}\u00fc\u00dc\u00f1\u00d1\u00e1\u00e9\u00ed\u00f3\u00fa\u00c1\u00c9\u00cd\u00d3\u00da]+",
             perl = TRUE),
    use.names = FALSE
  )
  raw <- raw[nzchar(raw)]
  out <- vapply(raw, clean_word, character(1), lang = lang)
  out <- out[!is.na(out) & nzchar(out)]
  unname(out)
}
