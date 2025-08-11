#' Contar oraciones en un texto
#'
#' Calcula la cantidad de oraciones por cada elemento de un vector de texto,
#' utilizando \code{\link{segment_sentences}} para la segmentación. Permite
#' definir cómo tratar valores \code{NA} y si se deben descartar oraciones vacías
#' (p. ej., producidas por espacios o puntuación aislada).
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar.
#' @param na_as_zero Lógico; si es \code{TRUE}, cuando \code{text} tiene \code{NA}
#'   devuelve 0 en lugar de \code{NA} en el conteo.
#' @param drop_empty Lógico; si es \code{TRUE}, descarta segmentos vacíos o con
#'   solo espacios al contar.
#'
#' @return Un vector entero con la cantidad de oraciones por elemento de \code{text}.
#'
#' @details
#' Esta función es un contenedor liviano que delega la lógica de segmentación
#' en \code{\link{segment_sentences}} y solo realiza el conteo. Si necesitás
#' reglas distintas de corte, personalizá \code{segment_sentences()}.
#'
#' @examples
#' count_sentences("Hola. ¿Cómo estás? ¡Todo bien!")
#' count_sentences(c("Una sola.", "Dos. Sí."))         # 1, 2
#' count_sentences(NA_character_, na_as_zero = TRUE)    # 0
#'
#' @seealso \code{\link{segment_sentences}}
#' @export
count_sentences <- function(text,
                            na_as_zero = FALSE,
                            drop_empty = TRUE) {
  if (!is.character(text)) {
    stop("`text` debe ser un vector de caracteres.", call. = FALSE)
  }

  # Manejo explícito de NA para respetar `na_as_zero`
  is_na <- is.na(text)

  # Para no propagar NAs al segmentar, reemplazamos por cadena vacía temporalmente
  text_safe <- text
  text_safe[is_na] <- ""

  segs <- segment_sentences(
    text_safe,
    keep_delim = TRUE,
    normalize_spacing = TRUE
  )

  counts <- vapply(segs, function(s) {
    if (drop_empty) {
      s_trim <- trimws(s)
      # Quitar toda puntuación Unicode (incluye ., !, ?, …, comillas, etc.)
      s_nopunct <- gsub("\\p{P}+", "", s_trim, perl = TRUE)
      # Quedarse solo con segmentos que tengan letras/dígitos tras limpiar
      s <- s_trim[nzchar(s_nopunct)]
    }
    length(s)
  }, integer(1))


  # Restaurar NAs según preferencia
  if (!na_as_zero) {
    counts[is_na] <- NA_integer_
  }

  counts
}
