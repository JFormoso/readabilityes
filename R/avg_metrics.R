#' Métricas promedio por palabra
#'
#' Conjunto de utilidades para calcular promedios por palabra a partir de un
#' texto: (a) sílabas promedio por palabra, (b) letras promedio por palabra y
#' (c) caracteres promedio por palabra. Estas funciones delegan la separación en
#' palabras a \code{\link{tokenize_words}} y, cuando corresponde, el conteo a
#' \code{\link{count_words}} y \code{\link{count_syllables}}.
#'
#' @name avg_metrics
NULL

#' Sílabas promedio por palabra
#'
#' Calcula el promedio de sílabas por palabra para cada elemento de \code{text}.
#' Internamente suma las sílabas con \code{\link{count_syllables}} y divide por
#' la cantidad de palabras con \code{\link{count_words}}.
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar.
#' @param na_as_zero Lógico; si es \code{TRUE}, cuando \code{text} tiene \code{NA}
#'   devuelve 0 en lugar de \code{NA}. Además, si un elemento no contiene palabras,
#'   devuelve 0 en lugar de \code{NA}.
#' @param drop_empty Lógico; si es \code{TRUE}, descarta tokens vacíos antes de
#'   contar (se reenvía a las funciones de conteo/tokenización).
#' @param syllable_fun Función de silabificación por palabra (por defecto
#'   \code{syll_count}); se reenvía a \code{\link{count_syllables}}.
#' @param syll_args Lista de argumentos adicionales para \code{syllable_fun}.
#' @param ... Argumentos adicionales que se pasan a \code{\link{tokenize_words}}.
#'
#' @return Un vector numérico con el promedio de sílabas por palabra.
#'
#' @examples
#' avg_syllables_per_word("Este es un texto de prueba.")
#' avg_syllables_per_word(c("Una sola", "Dos palabras más"))
#'
#' @seealso \code{\link{count_syllables}}, \code{\link{count_words}}, \code{\link{tokenize_words}}
#' @export
avg_syllables_per_word <- function(text,
                                   na_as_zero = FALSE,
                                   drop_empty = TRUE,
                                   syllable_fun = syll_count,
                                   syll_args = list(),
                                   ...) {
  if (!is.character(text)) {
    stop("`text` debe ser un vector de caracteres.", call. = FALSE)
  }

  # Normalización defensiva
  na_as_zero <- isTRUE(na_as_zero)
  drop_empty <- isTRUE(drop_empty)

  # Manejo de NAs
  is_na <- is.na(text)

  # Conteos
  total_syll <- count_syllables(
    text,
    syllable_fun = syllable_fun,
    na_as_zero   = na_as_zero,
    drop_empty   = drop_empty,
    syll_args    = syll_args,
    ...
  )
  total_words <- count_words(
    text,
    na_as_zero = na_as_zero,
    drop_empty = drop_empty,
    ...
  )

  # Promedio con manejo de divisiones por cero
  out <- ifelse(total_words > 0, total_syll / total_words, NA_real_)

  # Restaurar NAs y opción na_as_zero
  out[is.na(total_words) | is.na(total_syll)] <- NA_real_
  if (na_as_zero) {
    out[is.na(out)] <- 0
  }

  out
}

#' Letras promedio por palabra
#'
#' Calcula el promedio de **letras** por palabra (sólo letras Unicode, excluye
#' dígitos y signos de puntuación) para cada elemento de \code{text}. La
#' tokenización se realiza con \code{\link{tokenize_words}}.
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar.
#' @param na_as_zero Lógico; si es \code{TRUE}, cuando \code{text} tiene \code{NA}
#'   devuelve 0 en lugar de \code{NA}. Además, si un elemento no contiene palabras,
#'   devuelve 0 en lugar de \code{NA}.
#' @param drop_empty Lógico; si es \code{TRUE}, descarta tokens vacíos antes de contar.
#' @param ... Argumentos adicionales que se pasan a \code{\link{tokenize_words}}.
#'
#' @return Un vector numérico con el promedio de letras por palabra.
#'
#' @details
#' El conteo de letras se realiza usando la clase Unicode \code{\\p{L}}. Si
#' \code{strip_punct = TRUE} en \code{\link{tokenize_words}} (valor por defecto),
#' la mayor parte de la puntuación se elimina antes del conteo.
#'
#' @examples
#' letters_per_word("Hola mundo!")
#' letters_per_word(c("N° 123", "Palabra-compuesta"))
#'
#' @seealso \code{\link{tokenize_words}}, \code{\link{count_words}}
#' @export
letters_per_word <- function(text,
                             na_as_zero = FALSE,
                             drop_empty = TRUE,
                             ...) {
  if (!is.character(text)) {
    stop("`text` debe ser un vector de caracteres.", call. = FALSE)
  }

  is_na <- is.na(text)
  text_safe <- text
  text_safe[is_na] <- ""

  tokens_list <- tokenize_words(text_safe, ...)
  res <- vapply(tokens_list, function(tks) {
    if (drop_empty) {
      tks <- trimws(tks)
      tks <- tks[nzchar(tks)]
    }
    n_words <- length(tks)
    if (!n_words) return(NA_real_)

    # Contar sólo letras Unicode por token
    # (stringi facilita el conteo con propiedades Unicode)
    letter_counts <- stringi::stri_count_regex(tks, "\\p{L}")
    sum(letter_counts) / n_words
  }, numeric(1))

  # Restaurar NAs y opción na_as_zero
  res[is_na] <- NA_real_
  if (na_as_zero) {
    res[is.na(res)] <- 0
  }

  res
}

#' Caracteres promedio por palabra
#'
#' Calcula el promedio de **caracteres** por palabra para cada elemento de
#' \code{text}. Por defecto, se contabilizan todos los caracteres presentes en
#' cada token (tal como fueron devueltos por \code{\link{tokenize_words}}),
#' por lo que normalmente no incluye puntuación si \code{strip_punct = TRUE}.
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar.
#' @param na_as_zero Lógico; si es \code{TRUE}, cuando \code{text} tiene \code{NA}
#'   devuelve 0 en lugar de \code{NA}. Además, si un elemento no contiene palabras,
#'   devuelve 0 en lugar de \code{NA}.
#' @param drop_empty Lógico; si es \code{TRUE}, descarta tokens vacíos antes de contar.
#' @param ... Argumentos adicionales que se pasan a \code{\link{tokenize_words}}.
#'
#' @return Un vector numérico con el promedio de caracteres por palabra.
#'
#' @details
#' El conteo de caracteres se realiza con \code{nchar(..., type = "chars")}.
#' Si deseás excluir explícitamente ciertos signos (p. ej., guiones), asegurate
#' de configurar \code{strip_punct} o \code{keep_hyphens} en
#' \code{\link{tokenize_words}} según corresponda.
#'
#' @examples
#' chars_per_word("Hola mundo!")                 # típico: ~4 y ~5
#' chars_per_word("Palabra-compuesta ejemplo")   # depende de `keep_hyphens`
#'
#' @seealso \code{\link{tokenize_words}}, \code{\link{count_words}}
#' @export
chars_per_word <- function(text,
                           na_as_zero = FALSE,
                           drop_empty = TRUE,
                           ...) {
  if (!is.character(text)) {
    stop("`text` debe ser un vector de caracteres.", call. = FALSE)
  }

  is_na <- is.na(text)
  text_safe <- text
  text_safe[is_na] <- ""

  tokens_list <- tokenize_words(text_safe, ...)
  res <- vapply(tokens_list, function(tks) {
    if (drop_empty) {
      tks <- trimws(tks)
      tks <- tks[nzchar(tks)]
    }
    n_words <- length(tks)
    if (!n_words) return(NA_real_)

    char_counts <- nchar(tks, type = "chars", allowNA = FALSE, keepNA = FALSE)
    sum(char_counts) / n_words
  }, numeric(1))

  # Restaurar NAs y opción na_as_zero
  res[is_na] <- NA_real_
  if (na_as_zero) {
    res[is.na(res)] <- 0
  }

  res
}
