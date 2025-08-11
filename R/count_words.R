#' Contar palabras en un texto
#'
#' Calcula la cantidad de palabras por cada elemento de un vector de texto,
#' utilizando \code{\link{tokenize_words}} para la tokenización. Permite definir
#' cómo tratar valores \code{NA} y si se deben descartar tokens vacíos.
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar.
#' @param na_as_zero Lógico; si es \code{TRUE}, cuando \code{text} tiene \code{NA}
#'   devuelve 0 en lugar de \code{NA} en el conteo.
#' @param drop_empty Lógico; si es \code{TRUE}, descarta tokens vacíos o con solo
#'   espacios al contar.
#' @param ... Argumentos adicionales que se pasan a \code{\link{tokenize_words}}.
#'
#' @return Un vector entero con la cantidad de palabras por elemento de \code{text}.
#'
#' @details
#' Esta función es un contenedor liviano que delega la lógica de separación en
#' \code{\link{tokenize_words}} y solo realiza el conteo. Si necesitás reglas
#' distintas de tokenización (p. ej., incluir o excluir guiones, números o tildes),
#' ajustá los argumentos de \code{\link{tokenize_words}}.
#'
#' @examples
#' count_words("Este es un ejemplo sencillo.")
#' count_words(c("Primera frase.", "Segunda frase con más palabras."))
#' count_words(NA_character_, na_as_zero = TRUE)  # devuelve 0
#'
#' @seealso \code{\link{tokenize_words}}
#' @export
# En count_words()

count_words <- function(text,
                        na_as_zero = FALSE,
                        drop_empty = TRUE,
                        ...) {
  if (!is.character(text)) {
    stop("`text` debe ser un vector de caracteres.", call. = FALSE)
  }

  # 🔧 Normalización defensiva (evita errores si llegan valores no-lógicos)
  na_as_zero <- isTRUE(na_as_zero)
  drop_empty <- isTRUE(drop_empty)

  # Manejo de NAs
  is_na <- is.na(text)
  text_safe <- text
  text_safe[is_na] <- ""

  tokens <- tokenize_words(text_safe, ...)

  counts <- vapply(tokens, function(tks) {
    if (drop_empty) {
      tks <- trimws(tks)
      tks <- tks[nzchar(tks)]
    }
    length(tks)
  }, integer(1))

  # Restaurar NAs si corresponde
  if (!na_as_zero) {
    counts[is_na] <- NA_integer_
  }

  counts
}
