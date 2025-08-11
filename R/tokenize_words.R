#' Tokenizar texto en palabras
#'
#' Divide un vector de texto en tokens individuales de palabras, utilizando
#' reglas adaptadas para texto en español. La función puede, opcionalmente,
#' normalizar el uso de mayúsculas, conservar o eliminar tildes, y decidir cómo
#' tratar guiones y números.
#'
#' @param text Vector de caracteres que contiene el texto a tokenizar.
#' @param lowercase Lógico; si es \code{TRUE}, convierte todo el texto a minúsculas antes de tokenizar.
#' @param keep_accents Lógico; si es \code{FALSE}, se eliminan las tildes de los caracteres.
#' @param strip_punct Lógico; si es \code{TRUE}, elimina la puntuación antes de tokenizar.
#' @param keep_hyphens Lógico; si es \code{TRUE}, conserva los guiones dentro de las palabras (por ejemplo, \emph{teórico-práctico}).
#' @param remove_numbers Lógico; si es \code{TRUE}, elimina los tokens que son puramente numéricos.
#'
#' @return Una lista donde cada elemento corresponde a los tokens obtenidos de cada elemento de \code{text}.
#' @examples
#' tokenize_words("Este es un texto de prueba, con tildes y números: 123.")
#' tokenize_words(c("Primera frase.", "Segunda frase con más palabras."))
#' @export
tokenize_words <- function(text,
                           lowercase = TRUE,
                           keep_accents = TRUE,
                           strip_punct = TRUE,
                           keep_hyphens = TRUE,
                           remove_numbers = FALSE,
                           strip_symbols = FALSE,
                           flatten = FALSE) {
  if (!is.character(text)) {
    stop("`text` debe ser un vector de caracteres.", call. = FALSE)
  }

  if (lowercase) {
    text <- tolower(text)
  }

  if (!keep_accents) {
    text <- stringi::stri_trans_general(text, "Latin-ASCII")
  }

  # --- NUEVO: limpieza Unicode y preservación de guiones ---
  # \p{P} = puntuación Unicode (incluye ¡ ¿ — …)
  # \p{S} = símbolos Unicode (emojis, divisas, etc.)
  if (strip_punct) {
    if (keep_hyphens) {
      # proteger guiones antes de limpiar puntuación
      text <- gsub("-", "\uF000", text, fixed = TRUE)
      text <- gsub("\\p{P}+", " ", text, perl = TRUE)
      text <- gsub("\uF000", "-", text, fixed = TRUE)
    } else {
      text <- gsub("\\p{P}+", " ", text, perl = TRUE)
    }
  }

  if (strip_symbols) {
    text <- gsub("\\p{S}+", " ", text, perl = TRUE)
  }
  # --- FIN NUEVO ---

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
