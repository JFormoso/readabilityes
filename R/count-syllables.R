#' Contar sílabas en un texto
#'
#' Calcula la cantidad total de sílabas por cada elemento de un vector de
#' texto. Primero tokeniza con \code{\link{tokenize_words}} y luego suma las
#' sílabas de cada palabra con \code{syllable_fun} (por defecto,
#' \code{syll_count}).
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar.
#' @param lang Código de idioma: `"es"` (español, por defecto) o `"ca"`
#'   (catalán). Se pasa a \code{\link{tokenize_words}} y a
#'   \code{syllable_fun}. Si no se especifica, se usa
#'   \code{syll_get_option("lang")}.
#' @param syllable_fun Función que recibe una palabra y devuelve el número de
#'   sílabas. Por defecto \code{syll_count}. Debe aceptar un argumento `lang`
#'   si se quiere usar con múltiples idiomas.
#' @param na_as_zero Lógico; si es \code{TRUE}, los \code{NA} en \code{text}
#'   devuelven 0 en lugar de \code{NA}.
#' @param drop_empty Lógico; si es \code{TRUE}, descarta tokens vacíos antes
#'   de contar.
#' @param syll_args Lista de argumentos adicionales para \code{syllable_fun},
#'   además de `lang` (que se pasa automáticamente).
#' @param ... Argumentos adicionales para \code{\link{tokenize_words}}.
#'
#' @return Vector entero con la cantidad total de sílabas por elemento de
#'   \code{text}.
#'
#' @examples
#' count_syllables("Este es un texto de prueba.")
#' count_syllables("L'Anna va a l'escola", lang = "ca")
#' count_syllables(c("Uno dos", NA), na_as_zero = TRUE)
#'
#' @seealso \code{\link{tokenize_words}}, \code{\link{syll_count}}
#' @export
count_syllables <- function(text,
                            lang         = syll_get_option("lang"),
                            syllable_fun = syll_count,
                            na_as_zero   = FALSE,
                            drop_empty   = TRUE,
                            syll_args    = list(),
                            ...) {
  if (!is.character(text)) {
    stop("`text` debe ser un vector de caracteres.", call. = FALSE)
  }
  if (!is.function(syllable_fun)) {
    stop("`syllable_fun` debe ser una funci\u00f3n.", call. = FALSE)
  }
  if (!is.list(syll_args)) {
    stop("`syll_args` debe ser una lista.", call. = FALSE)
  }

  na_as_zero <- isTRUE(na_as_zero)
  drop_empty <- isTRUE(drop_empty)

  is_na     <- is.na(text)
  text_safe <- text
  text_safe[is_na] <- ""

  # Tokenización con idioma correspondiente
  tokens <- tokenize_words(text_safe, lang = lang, ...)

  counts <- vapply(tokens, function(tks) {
    if (drop_empty) {
      tks <- trimws(tks)
      tks <- tks[nzchar(tks)]
    }
    if (!length(tks)) return(0L)

    syls <- vapply(
      tks,
      function(w) {
        val <- {
          fn_formals <- names(formals(syllable_fun))
          call_args <- list(w)
          if ("lang" %in% fn_formals || "..." %in% fn_formals) {
            call_args <- c(call_args, list(lang = lang))
          }
          call_args <- c(call_args, syll_args)
          do.call(syllable_fun, call_args)
        }
        if (!is.numeric(val) || length(val) != 1L || is.na(val)) 0L
        else as.integer(round(val))
      },
      integer(1)
    )
    sum(syls)
  }, integer(1))

  if (!na_as_zero) {
    counts[is_na] <- NA_integer_
  }

  counts
}
