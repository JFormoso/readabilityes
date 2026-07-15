#' Tokenizar texto en palabras
#'
#' Divide un vector de texto en tokens individuales de palabras. Soporta
#' reglas específicas por idioma: en catalán (`lang = "ca"`) los apóstrofes
#' se preservan por defecto ya que forman parte de contracciones léxicas
#' (p. ej., \emph{l'Anna}, \emph{perdre'l}).
#'
#' @param text Vector de caracteres que contiene el texto a tokenizar.
#' @param lang Código de idioma: `"es"` (español, por defecto) o `"ca"`
#'   (catalán). Si no se especifica, se usa `syll_get_option("lang")`.
#'   En catalán activa `keep_apostrophes = TRUE` por defecto.
#' @param lowercase Lógico; si es \code{TRUE}, convierte todo el texto a
#'   minúsculas antes de tokenizar.
#' @param keep_accents Lógico; si es \code{FALSE}, se eliminan las tildes.
#' @param strip_punct Lógico; si es \code{TRUE}, elimina la puntuación
#'   (\code{\\p{P}}) antes de tokenizar.
#' @param keep_hyphens Lógico; si es \code{TRUE}, conserva los guiones dentro
#'   de las palabras (p. ej., \emph{agradar-me}).
#' @param keep_apostrophes Lógico; si es \code{TRUE}, conserva los apóstrofes
#'   dentro de los tokens (p. ej., \emph{l'Anna}, \emph{perdre'l}). Por
#'   defecto es \code{TRUE} cuando \code{lang = "ca"} y \code{FALSE} en caso
#'   contrario.
#' @param remove_numbers Lógico; si es \code{TRUE}, elimina los tokens
#'   puramente numéricos.
#' @param strip_symbols Lógico; si es \code{TRUE}, elimina símbolos Unicode
#'   (\code{\\p{S}}, p. ej., emojis, divisas).
#' @param flatten Lógico; si es \code{TRUE} y \code{text} tiene longitud 1,
#'   devuelve un vector en lugar de una lista.
#'
#' @return
#' - Si \code{length(text) > 1}: lista donde cada elemento contiene los tokens
#'   del elemento correspondiente de \code{text}.
#' - Si \code{length(text) == 1} y \code{flatten = TRUE}: vector de caracteres.
#' - En cualquier otro caso: lista de tokens.
#'
#' @details
#' La limpieza de puntuación usa la clase Unicode \code{\\p{P}}.
#' Los apóstrofes (`'` y `\u2019`) se tratan por separado de la puntuación
#' general cuando \code{keep_apostrophes = TRUE}.
#' Si \code{strip_punct = TRUE} y \code{keep_hyphens = TRUE}, los guiones
#' internos se preservan.
#'
#' @examples
#' tokenize_words("Este es un texto de prueba, con tildes y números: 123.")
#' tokenize_words(c("Primera frase.", "Segona frase en català."), lang = "ca")
#' tokenize_words("l'Anna va a l'escola", lang = "ca", flatten = TRUE)
#' tokenize_words("agradar-me", lang = "ca", flatten = TRUE)
#'
#' @export
tokenize_words <- function(text,
                           lang = syll_get_option("lang"),
                           lowercase  = TRUE,
                           keep_accents  = TRUE,
                           strip_punct  = TRUE,
                           keep_hyphens  = TRUE,
                           keep_apostrophes = (lang == "ca"),
                           remove_numbers = FALSE,
                           strip_symbols  = TRUE,
                           flatten  = FALSE) {

  if (!is.character(text)) {
    stop("`text` debe ser un vector de caracteres.", call. = FALSE)
  }

  if (lowercase) {
    text <- tolower(text)
  }

  if (!keep_accents) {
    text <- stringi::stri_trans_general(text, "Latin-ASCII")
  }

  if (strip_punct) {
    # Proteger guiones internos si corresponde
    if (keep_hyphens) {
      text <- gsub("-", "\uF000", text, fixed = TRUE)
    }

    # Proteger apóstrofes internos si corresponde
    # Solo los que están entre caracteres de palabra (no al inicio/fin de texto)
    if (keep_apostrophes) {
      # Apóstrofe recto y tipográfico
      text <- gsub("'",       "\uF001", text, fixed = TRUE)
      text <- gsub("\u2019",  "\uF001", text, fixed = TRUE)
    }

    # Eliminar puntuación Unicode restante
    text <- gsub("\\p{P}+", " ", text, perl = TRUE)

    # Restaurar guiones protegidos
    if (keep_hyphens) {
      text <- gsub("\uF000", "-", text, fixed = TRUE)
    }

    # Restaurar apóstrofes protegidos (normalizados a apóstrofe recto)
    if (keep_apostrophes) {
      text <- gsub("\uF001", "'", text, fixed = TRUE)
    }
  }

  if (strip_symbols) {
    text <- gsub("\\p{S}+", " ", text, perl = TRUE)
  }

  tokens_list <- strsplit(text, "\\s+", perl = TRUE)

  tokens_list <- lapply(tokens_list, function(tokens) {
    tokens <- tokens[tokens != ""]
    if (remove_numbers) {
      tokens <- tokens[!grepl("^[0-9]+$", tokens)]
    }
    tokens
  })

  if (flatten && length(tokens_list) == 1L) {
    return(tokens_list[[1L]])
  }
  tokens_list
}
