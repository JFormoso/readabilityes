#' Segmentar texto en oraciones
#'
#' Divide un vector de caracteres en oraciones usando reglas adaptadas para
#' español. La función protege abreviaturas comunes (p. ej., \emph{Sr.}, \emph{Dra.},
#' \emph{etc.}), iniciales (\emph{J. R. R.}), números con separador decimal
#' (punto o coma) y elipsis (\code{...}) antes de realizar el corte. También
#' permite agregar abreviaturas propias.
#'
#' @param text Vector de caracteres con el texto a segmentar.
#' @param extra_abbr Vector de caracteres opcional con abreviaturas adicionales
#'   (sin el punto final), por ejemplo \code{c("coord", "aprox")}.
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
#' segment_sentences("J. R. R. Tolkien escribió mucho. Etc. fin.")
#' segment_sentences("Ver págs. 12-14, cap. 3. Además, ver apéndices.")
#' segment_sentences("Aprox. 3,5 km. Luego continuar.")
#'
#' # Con abreviaturas personalizadas:
#' segment_sentences("Coord. Gral. del proyecto. Reunión mañana.",
#'                   extra_abbr = c("Coord", "Gral"))
#' @export
segment_sentences <- function(text,
                              extra_abbr = NULL,
                              keep_delim = TRUE,
                              normalize_spacing = TRUE) {
  if (!is.character(text)) {
    stop("`text` debe ser un vector de caracteres.", call. = FALSE)
  }

  # Abreviaturas comunes en español SIN el punto final
  base_abbr <- c(
    # Tratamientos y títulos
    "sr", "sra", "srta", "dr", "dra", "lic", "ing", "arq", "prof",
    # Pronominales/respeto
    "ud", "uds",
    # Editoriales y académicas
    "etc", "p", "pp", "cap", "caps", "fig", "figs", "p\u00e1g", "p\u00e1gs",
    "pag", "pags", "n\u00fam", "num", "\u00ba", "vol", "vols", "ed",
    # Varias
    "av", "dto", "dpto", "ej", "aprox", "tel", "dir", "coord", "gral",
    # Latinas frecuentes (sin punto para normalizar)
    "e\\.g", "i\\.e", "vs"
  )

  # Normalizar abreviaturas extra
  if (!is.null(extra_abbr)) {
    stopifnot(is.character(extra_abbr))
    extra_abbr <- tolower(extra_abbr)
    base_abbr <- unique(c(base_abbr, extra_abbr))
  }

  # Tokens de protección
  DOT_TKN   <- "\u2414"  # ␔ para puntos protegidos
  ELL_TKN   <- "\u2416"  # ␖ para elipsis
  QMRK_TKN  <- "\u2418"  # ␘ (reservado)

  protect_abbrev <- function(x) {
    # Alternancia de abreviaturas (sin punto final)
    abbr_pat <- paste0("(?:", paste0(base_abbr, collapse = "|"), ")")

    # FIX: sin lookbehind variable — usamos captura + lookbehind de 1 char (no letra)
    # Unicode-aware: (*UCP), case-insensitive: (?i)
    pat <- paste0("(*UCP)(?i)(?<!\\p{L})(", abbr_pat, ")\\.")
    x <- gsub(pat, paste0("\\1", DOT_TKN), x, perl = TRUE)

    # Proteger iniciales tipo "J. R. R." (lookbehind de longitud fija = 1)
    x <- gsub("(?<=\\b[[:alpha:]])\\.", DOT_TKN, x, perl = TRUE)

    # Proteger elipsis ...
    x <- gsub("\\.{3,}", ELL_TKN, x, perl = TRUE)

    # Proteger separadores decimales . entre dígitos (lookaround fijo)
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

    # Insertar separador tras delimitador si keep_delim = TRUE
    if (keep_delim) {
      x_marked <- gsub("([\\.\\!\\?])(\\s+|$)", "\\1\u241E", x_prot, perl = TRUE)
      parts <- strsplit(x_marked, "\u241E", fixed = TRUE)[[1]]
    } else {
      parts <- unlist(strsplit(x_prot, "(?<=[\\.\\!\\?])\\s+", perl = TRUE))
    }

    # Restaurar tokens
    parts <- unprotect_all(parts)

    # Si no conservamos delimitador, quitarlos del final
    if (!keep_delim) {
      parts <- sub("\\s*[\\.!\\?]+$", "", parts, perl = TRUE)
    }

    # Normalizar espacios
    if (normalize_spacing) {
      parts <- gsub("\\s+", " ", parts, perl = TRUE)
      parts <- trimws(parts)
    }

    # Eliminar vacíos
    parts <- parts[nzchar(parts)]
    parts
  }

  lapply(text, split_sentences)
}

