#' Índices de legibilidad para textos en español (y, a futuro, catalán)
#'
#' Calcula uno o varios índices de legibilidad sobre un vector de texto,
#' devolviendo un \code{data.frame} con una fila por texto. Reemplaza el
#' patrón anterior de una función exportada por fórmula
#' (\code{szigriszt_pazos()}, etc.) por una única interfaz genérica,
#' inspirada en \code{quanteda::textstat_readability()}.
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar. Si tiene
#'   nombres, se incluyen como columna \code{doc_id} en el resultado.
#' @param index Vector de caracteres con los nombres de índice a calcular
#'   (p. ej. \code{c("szigriszt_pazos", "crawford")}). Si es \code{NULL}
#'   (por defecto), calcula todos los índices válidos para \code{lang}.
#' @param lang Código de idioma: `"es"` (por defecto) o `"ca"`. Determina
#'   qué índices están disponibles — pedir un índice no válido para
#'   \code{lang} produce un error explícito, no un resultado silenciosamente
#'   incorrecto.
#' @param intermediate Lógico; si es \code{TRUE}, agrega columnas
#'   \code{n_words}, \code{n_sentences}, \code{n_syllables} con las
#'   cantidades intermedias usadas en el cálculo.
#' @param include_category Lógico (por defecto \code{FALSE}, opt-in); si es
#'   \code{TRUE}, agrega una columna \code{<índice>_category} para cada
#'   índice pedido que tenga una categorización asociada (p. ej. INFLESZ
#'   para Szigriszt-Pazos). Los índices sin categoría no agregan columna.
#' @param ... Reservado para uso futuro.
#'
#' @return Un \code{data.frame} con una fila por elemento de \code{text},
#'   una columna por índice pedido, y opcionalmente \code{doc_id},
#'   columnas intermedias y columnas de categoría.
#'
#' @examples
#' readability_index("Este es un texto de prueba.", lang = "es")
#' readability_index(c("Texto uno.", "Texto dos, un poco más largo."),
#'                   index = "szigriszt_pazos", lang = "es")
#' readability_index("Este es un texto de prueba.", lang = "es",
#'                   index = "szigriszt_pazos", include_category = TRUE)
#'
#' # Índice no válido para el idioma pedido: error explícito, no silencio
#' \dontrun{
#' readability_index("Text en catal\u00e0.", lang = "ca")
#' }
#' @export
readability_index <- function(text,
                              index            = NULL,
                              lang             = syll_get_option("lang"),
                              intermediate     = FALSE,
                              include_category = FALSE,
                              ...) {
  if (!is.character(text)) {
    stop("`text` debe ser un vector de caracteres.", call. = FALSE)
  }
  .chk_lang(lang)

  registry <- .readability_formula_registry

  # Determinar qué índices calcular
  valid_for_lang <- names(registry)[
    vapply(registry, function(e) lang %in% e$langs, logical(1))
  ]

  if (is.null(index)) {
    chosen <- valid_for_lang
    if (!length(chosen)) {
      stop(sprintf("No hay \u00edndices de legibilidad implementados para '%s' todav\u00eda.", lang),
           call. = FALSE)
    }
  } else {
    unknown <- setdiff(index, names(registry))
    if (length(unknown)) {
      stop(sprintf("\u00cdndice(s) desconocido(s): %s. Usar uno de: %s.",
                   paste(unknown, collapse = ", "),
                   paste(names(registry), collapse = ", ")),
           call. = FALSE)
    }
    not_valid_lang <- setdiff(index, valid_for_lang)
    if (length(not_valid_lang)) {
      stop(sprintf("\u00cdndice(s) no v\u00e1lido(s) para lang = '%s': %s.",
                   lang, paste(not_valid_lang, collapse = ", ")),
           call. = FALSE)
    }
    chosen <- index
  }

  # Calcular inputs: siempre W/S/Sy/C (baratos, ya se necesitan para
  # intermediate=TRUE); n/mean_len/var_len (Mu) solo si hace falta,
  # porque implica tokenizar palabra por palabra de nuevo.
  vals <- .readability_intermediate(text, lang = lang)

  needs_mu_stats <- any(vapply(chosen, function(idx) {
    any(registry[[idx]]$inputs %in% c("n", "mean_len", "var_len"))
  }, logical(1)))
  if (needs_mu_stats) {
    vals <- c(vals, .mu_stats(text, lang = lang))
  }

  # Calcular cada índice pedido
  scores <- lapply(chosen, function(idx) {
    entry <- registry[[idx]]
    do.call(entry$fn, vals[entry$inputs])
  })
  names(scores) <- chosen

  out <- as.data.frame(scores, stringsAsFactors = FALSE)

  if (include_category) {
    for (idx in chosen) {
      cat_fn <- registry[[idx]]$category_fn
      if (!is.null(cat_fn)) {
        out[[paste0(idx, "_category")]] <- cat_fn(scores[[idx]])
      }
    }
  }

  if (intermediate) {
    out <- cbind(
      data.frame(n_words = vals$W, n_sentences = vals$S, n_syllables = vals$Sy),
      out
    )
  }

  if (!is.null(names(text))) {
    out <- cbind(data.frame(doc_id = names(text), stringsAsFactors = FALSE), out)
  }

  out
}
