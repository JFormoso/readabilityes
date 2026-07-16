#' Resumen de legibilidad por texto
#'
#' Calcula métricas de legibilidad para cada fila de un data frame con
#' columnas `id_text` y `text`. Es una capa fina sobre
#' \code{\link{readability_index}}: no reimplementa el conteo ni la
#' tokenización, solo adapta la entrada/salida al formato con id.
#'
#' @param data Data frame con columnas `id_text` y `text`.
#' @param lang Código de idioma: `"es"` (por defecto) o `"ca"`.
#' @param index Vector de caracteres con los índices a calcular. Por
#'   defecto, \code{c("szigriszt_pazos", "gutierrez_de_polini")} — el
#'   mismo conjunto que devolvía la versión anterior de esta función.
#'   Podés pedir cualquier índice registrado (p. ej. agregar
#'   \code{"fernandez_huerta"}, \code{"crawford"} o \code{"mu"}).
#' @param include_category Lógico (por defecto \code{TRUE}, a diferencia
#'   de \code{readability_index()} donde es opt-in) — se mantiene
#'   \code{TRUE} acá para preservar la columna \code{inflesz_category}
#'   que ya devolvía la versión anterior.
#' @param ... Reservado para uso futuro.
#'
#' @return Un \code{tibble} con una fila por texto: \code{id_text},
#'   \code{n_words}, \code{n_syllables}, \code{n_sentences},
#'   \code{avg_syllables_per_word}, \code{letters_per_word}, y una
#'   columna por índice pedido (más su categoría si corresponde).
#'
#' @examples
#' df <- data.frame(id_text = c("a", "b"),
#'                  text = c("Texto uno.", "Otro texto, un poco más largo."))
#' readability_summary(df)
#' @export
readability_summary <- function(data,
                                lang             = syll_get_option("lang"),
                                index            = c("szigriszt_pazos", "gutierrez_de_polini"),
                                include_category = TRUE,
                                ...) {
  if (!is.data.frame(data)) {
    stop("`data` debe ser un data.frame.", call. = FALSE)
  }
  if (!all(c("id_text", "text") %in% names(data))) {
    stop("`data` debe tener columnas `id_text` y `text`.", call. = FALSE)
  }

  text <- as.character(data$text)
  id   <- data$id_text

  res <- readability_index(
    text,
    index            = index,
    lang             = lang,
    intermediate     = TRUE,
    include_category = include_category
  )

  avg_syll <- ifelse(res$n_words > 0, res$n_syllables / res$n_words, NA_real_)

  base_cols <- c("n_words", "n_syllables", "n_sentences", "letters_per_word")
  idx_cols  <- setdiff(names(res), base_cols)

  out <- tibble::tibble(
    id_text                = id,
    n_words                = as.integer(res$n_words),
    n_syllables             = as.integer(res$n_syllables),
    n_sentences             = as.integer(res$n_sentences),
    avg_syllables_per_word  = avg_syll,
    letters_per_word        = res$letters_per_word
  )

  tibble::as_tibble(cbind(out, res[idx_cols]))
}
