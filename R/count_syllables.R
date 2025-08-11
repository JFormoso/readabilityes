#' Contar sílabas en un texto
#'
#' Calcula la cantidad total de sílabas por cada elemento de un vector de texto.
#' Para ello, primero tokeniza el texto en palabras con \code{\link{tokenize_words}}
#' y luego suma las sílabas de cada palabra utilizando una función de
#' silabificación provista por el usuario o por el paquete (por defecto,
#' \code{syll_count}, definida en el núcleo de silabificación).
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar.
#' @param syllable_fun Función que recibe una palabra (cadena de caracteres) y
#'   devuelve el número de sílabas de esa palabra. Por defecto \code{syll_count}.
#' @param na_as_zero Lógico; si es \code{TRUE}, cuando \code{text} tiene \code{NA}
#'   devuelve 0 en lugar de \code{NA} en el conteo.
#' @param drop_empty Lógico; si es \code{TRUE}, descarta tokens vacíos o con solo
#'   espacios antes de contar sílabas.
#' @param syll_args Lista de argumentos adicionales que se pasarán a \code{syllable_fun}
#'   (útil si tu función de silabificación admite opciones, p. ej., normalización).
#' @param ... Argumentos adicionales que se pasan a \code{\link{tokenize_words}}.
#'
#' @return Un vector entero con la cantidad total de sílabas por elemento de \code{text}.
#'
#' @details
#' Esta función actúa como un contenedor: delega la separación en palabras a
#' \code{\link{tokenize_words}} y la estimación de sílabas por palabra a
#' \code{syllable_fun}. Si tu implementación de silabificación tiene otro nombre
#' (por ejemplo, \code{count_syllables_word} o \code{syllables_es}), pasala
#' mediante el argumento \code{syllable_fun}.
#'
#' @examples
#' # Supone que `syll_count()` existe y está disponible:
#' count_syllables("Este es un texto de prueba.")
#'
#' # Pasando una función de silabificación alternativa:
#' # count_syllables("Prueba alternativa.", syllable_fun = count_syllables_word)
#'
#' # Control de NAs:
#' count_syllables(c("Uno dos", NA), na_as_zero = TRUE)
#' # devuelve c(4, 0) si cada palabra tiene 2 sílabas
#'
#' @seealso \code{\link{tokenize_words}}, \code{syll_count}
#' @export
count_syllables <- function(text,
                            syllable_fun = syll_count,
                            na_as_zero = FALSE,
                            drop_empty = TRUE,
                            syll_args = list(),
                            ...) {
  if (!is.character(text)) {
    stop("`text` debe ser un vector de caracteres.", call. = FALSE)
  }
  if (!is.function(syllable_fun)) {
    stop("`syllable_fun` debe ser una funci\u00f3n que cuente s\u00edlabas de una palabra.", call. = FALSE)
  }
  if (!is.list(syll_args)) {
    stop("`syll_args` debe ser una lista de argumentos adicionales para `syllable_fun`.", call. = FALSE)
  }

  # Normalización defensiva
  na_as_zero <- isTRUE(na_as_zero)
  drop_empty <- isTRUE(drop_empty)

  # Manejo de NAs
  is_na <- is.na(text)
  text_safe <- text
  text_safe[is_na] <- ""

  # Tokenización (se pueden ajustar reglas vía `...`)
  tokens <- tokenize_words(text_safe, ...)

  # Conteo por elemento
  counts <- vapply(tokens, function(tks) {
    if (drop_empty) {
      tks <- trimws(tks)
      tks <- tks[nzchar(tks)]
    }
    if (!length(tks)) return(0L)

    # Contar sílabas por palabra usando la función provista
    syls <- vapply(
      tks,
      function(w) {
        # do.call permite pasar argumentos extra a la función de silabificación
        val <- do.call(syllable_fun, c(list(w), syll_args))
        if (!is.numeric(val) || length(val) != 1L || is.na(val)) 0L else as.integer(round(val))
      },
      integer(1)
    )
    sum(syls)
  }, integer(1))

  # Restaurar NAs si corresponde
  if (!na_as_zero) {
    counts[is_na] <- NA_integer_
  }

  counts
}
