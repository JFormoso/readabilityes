#' Segmentar texto en oraciones
#'
#' Divide un vector de caracteres en oraciones usando reglas adaptadas para
#' español (`"es"`) o catalán (`"ca"`). La función protege abreviaturas
#' comunes (p. ej., \emph{Sr.}, \emph{Dra.}, \emph{etc.}), iniciales
#' (\emph{J. R. R.}), números con separador decimal (punto o coma) y
#' elipsis (\code{...}) antes de realizar el corte. También permite agregar
#' abreviaturas propias.
#'
#' @param text Vector de caracteres con el texto a segmentar.
#' @param lang Código de idioma: `"es"` (por defecto) o `"ca"`. Determina
#'   qué lista de abreviaturas se usa para proteger puntos que no cierran
#'   oración.
#' @param extra_abbr Vector de caracteres opcional con abreviaturas
#'   adicionales (sin el punto final), por ejemplo \code{c("coord", "aprox")}.
#' @param keep_delim Lógico; si es \code{TRUE}, conserva el signo de cierre
#'   de oración (., ?, !) al final de cada segmento.
#' @param normalize_spacing Lógico; si es \code{TRUE}, normaliza espacios
#'   múltiples y recorta espacios en los extremos de cada oración.
#'
#' @return Una lista donde cada elemento contiene un vector de oraciones para
#'   el elemento correspondiente de \code{text}.
#'
#' @examples
#' segment_sentences("Hola. ¿Cómo estás? Todo bien, ¡gracias!")
#' segment_sentences("El Dr. Pérez llegó a las 10.30. Luego se retiró.")
#' segment_sentences("El Dr. Pérez va arribar a les 10.30.", lang = "ca")
#'
#' # Con abreviaturas personalizadas:
#' segment_sentences("Coord. Gral. del proyecto. Reunión mañana.",
#'                   extra_abbr = c("Coord", "Gral"))
#' @export
segment_sentences <- function(text,
                              lang = syll_get_option("lang"),
                              extra_abbr = NULL,
                              keep_delim = TRUE,
                              normalize_spacing = TRUE) {
  if (!is.character(text)) {
    stop("`text` debe ser un vector de caracteres.", call. = FALSE)
  }
  .chk_lang(lang)   # mensaje de error centralizado, mismo que config.R

  base_abbr <- .lang_get(lang)$sentence_abbr

  # Normalizar abreviaturas extra
  if (!is.null(extra_abbr)) {
    stopifnot(is.character(extra_abbr))
    extra_abbr <- tolower(extra_abbr)
    base_abbr <- unique(c(base_abbr, extra_abbr))
  }

  # Tokens de protección
  DOT_TKN   <- "\u2414"
  ELL_TKN   <- "\u2416"
  QMRK_TKN  <- "\u2418"

  protect_abbrev <- function(x) {
    if (length(base_abbr) == 0L) {
      # Idioma sin abreviaturas registradas: no proteger nada por ese lado,
      # pero igual proteger iniciales/elipsis/decimales más abajo.
      abbr_protected <- x
    } else {
      abbr_pat <- paste0("(?:", paste0(base_abbr, collapse = "|"), ")")
      pat <- paste0("(*UCP)(?i)(?<!\\p{L})(", abbr_pat, ")\\.")
      abbr_protected <- gsub(pat, paste0("\\1", DOT_TKN), x, perl = TRUE)
    }

    x <- gsub("(?<=\\b[[:alpha:]])\\.", DOT_TKN, abbr_protected, perl = TRUE)
    x <- gsub("\\.{3,}", ELL_TKN, x, perl = TRUE)
    x <- gsub("(?<=\\d)\\.(?=\\d)", DOT_TKN, x, perl = TRUE)
    x
  }

  unprotect_all <- function(x) {
    x <- gsub(DOT_TKN, ".", x, fixed = TRUE)
    x <- gsub(ELL_TKN, "...", x, fixed = TRUE)
    x
  }

  split_sentences <- function(x) {
    if (is.na(x) || x == "") return(character(0))

    x_prot <- protect_abbrev(x)

    if (keep_delim) {
      x_marked <- gsub("([\\.\\!\\?])(\\s+|$)", "\\1\u241E", x_prot, perl = TRUE)
      parts <- strsplit(x_marked, "\u241E", fixed = TRUE)[[1]]
    } else {
      parts <- unlist(strsplit(x_prot, "(?<=[\\.\\!\\?])\\s+", perl = TRUE))
    }

    parts <- unprotect_all(parts)

    if (!keep_delim) {
      parts <- sub("\\s*[\\.!\\?]+$", "", parts, perl = TRUE)
    }

    if (normalize_spacing) {
      parts <- gsub("\\s+", " ", parts, perl = TRUE)
      parts <- trimws(parts)
    }

    parts <- parts[nzchar(parts)]
    parts
  }

  lapply(text, split_sentences)
}
