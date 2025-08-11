# utils_clean.R — Normalización y limpieza de palabras
# -------------------------------------------------------------------

#' Limpia y normaliza una palabra (ES)
#'
#' Convierte a minúsculas, recorta espacios y elimina cualquier carácter
#' que no pertenezca al conjunto de letras válidas del paquete
#' (incluye tildes, ñ y diéresis). Si tras limpiar no queda nada, devuelve `NA`.
#'
#' @param word Cadena de texto (longitud 1). Si es `NA` o no es `character`,
#'   devuelve `NA`.
#' @return Cadena limpia o `NA_character_` si queda vacía.
#' @examples
#' clean_word("  Canci\u00f3n!  ")  # "canción"
#' clean_word("g\u00fciro")         # "güiro"
#' @export
#' @family syllabify-es
#' @seealso syllabify-internals
clean_word <- function(word) {
  if (length(word) != 1L || !is.character(word) || is.na(word)) {
    return(NA_character_)
  }
  w <- tolower(trimws(word))

  # Eliminamos todo lo que no sea letra válida según constants.R
  # (usa .valid_letters; .rx_valid_letters también está disponible en .onLoad)
  w <- gsub(sprintf("[^%s]", .valid_letters), "", w, perl = TRUE)

  if (identical(w, "")) NA_character_ else w
}

#' ¿La palabra contiene solo letras válidas?
#'
#' Verifica que todos los caracteres pertenezcan al alfabeto interno soportado.
#'
#' @param word Cadena de texto (longitud 1).
#' @return `TRUE`/`FALSE` o `NA` si `word` es inválida.
#' @examples
#' is_valid_word("pingüino")  # TRUE
#' is_valid_word("hola!")     # FALSE
#' @export
#' @family syllabify-es
is_valid_word <- function(word) {
  w <- clean_word(word)
  if (is.na(w)) return(NA)
  # Si al limpiar no cambia, entonces era 100% válida
  identical(w, tolower(trimws(word)))
}

#' Tokeniza y limpia un texto en palabras
#'
#' Divide un texto por separadores simples (espacios, guiones y puntuación),
#' limpia cada token y descarta vacíos.
#'
#' @param text Cadena de texto (puede contener varias palabras).
#' @return Vector de palabras limpias (puede ser de longitud 0).
#' @examples
#' tokenize_clean("¡Hola, mundo—cruel!")  # c("hola","mundo","cruel")
#' @export
#' @family syllabify-es
tokenize_clean <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) return(character())
  # Separadores comunes; luego limpiamos cada token
  raw <- unlist(strsplit(text, "[^\\p{L}\u00fc\u00dc\u00f1\u00d1\u00e1\u00e9\u00ed\u00f3\u00fa\u00c1\u00c9\u00cd\u00d3\u00da]+", perl = TRUE), use.names = FALSE)
  raw <- raw[nzchar(raw)]
  out <- vapply(raw, clean_word, character(1))
  out <- out[!is.na(out) & nzchar(out)]
  unname(out)
}
