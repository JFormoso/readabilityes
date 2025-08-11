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

  # Tokens de protección (caracteres poco probables en texto normal)
  DOT_TKN   <- "\u2414"  # ␔ para puntos protegidos
  ELL_TKN   <- "\u2416"  # ␖ para elipsis
  QMRK_TKN  <- "\u2418"  # ␘ (no siempre necesario, reservado)

  protect_abbrev <- function(x) {
    # Proteger abreviaturas: (\\bABBR)\\.
    # Construimos patrón para todas las abreviaturas, permitiendo variantes con acento y con/ sin escape.
    abbr_pat <- paste0("(?:", paste0(base_abbr, collapse = "|"), ")")
    # Reemplazar el punto final de la abreviatura por token
    x <- gsub(paste0("(?i)(?<=\\b", abbr_pat, ")\\."), DOT_TKN, x, perl = TRUE)

    # Proteger iniciales tipo "J. R. R." -> reemplazar puntos tras letras sueltas
    x <- gsub("(?<=\\b[[:alpha:]])\\.", DOT_TKN, x, perl = TRUE)

    # Proteger elipsis ...
    x <- gsub("\\.{3,}", ELL_TKN, x, perl = TRUE)

    # Proteger separadores decimales (tanto . como ,) entre dígitos
    x <- gsub("(?<=\\d)\\.(?=\\d)", DOT_TKN, x, perl = TRUE)

    x
  }

  unprotect_all <- function(x) {
    x <- gsub(DOT_TKN, ".", x, fixed = TRUE)
    x <- gsub(ELL_TKN, "...", x, fixed = TRUE)
    x
  }

  # División en oraciones:
  #  - Cortar en . ! ? cuando NO son tokens protegidos
  #  - Mantener el delimitador si keep_delim = TRUE
  split_sentences <- function(x) {
    if (is.na(x) || x == "") return(character(0))

    x_prot <- protect_abbrev(x)

    # Patrón de fin de oración: punto, signo de cierre ! o ?, que no sea token
    # Usamos corte por lookbehind en [.?!] seguido de espacio/fin de línea.
    # Para conservar el delimitador, capturamos el signo.
    if (keep_delim) {
      # Insertamos separador especial después del delimitador
      x_marked <- gsub("([\\.\\!\\?])(\\s+|$)", "\\1\u241E", x_prot, perl = TRUE)
      parts <- strsplit(x_marked, "\u241E", fixed = TRUE)[[1]]
    } else {
      parts <- unlist(strsplit(x_prot, "(?<=[\\.\\!\\?])\\s+", perl = TRUE))
    }

    # Restaurar tokens
    parts <- unprotect_all(parts)

    # si no conservamos delimitador, quitarlos del final
    if (!keep_delim) {
      parts <- sub("\\s*[\\.!\\?]+$", "", parts, perl = TRUE)
    }

    # Normalizar espacios
    if (normalize_spacing) {
      parts <- gsub("\\s+", " ", parts, perl = TRUE)
      parts <- trimws(parts)
    }

    # Eliminar vacíos residuales
    parts <- parts[nzchar(parts)]

    parts
  }

  # Aplicar a cada elemento del vector de entrada
  out <- lapply(text, split_sentences)
  out
}
