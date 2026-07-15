# ---------- Internos ----------

.cache_get <- function(key) {
  if (!exists(key, envir = .syll_cache, inherits = FALSE)) return(NULL)
  get(key, envir = .syll_cache, inherits = FALSE)
}

.cache_set <- function(key, value) {
  assign(key, value, envir = .syll_cache)
}

.cache_clear <- function() {
  rm(list = ls(envir = .syll_cache), envir = .syll_cache)
}

# ---------- API pública ----------

#' Gestión del caché de segmentaciones
#'
#' Funciones para manipular el caché interno que guarda segmentaciones silábicas
#' ya calculadas.
#'
#' @return
#' - `syll_cache_clear()` devuelve `invisible(NULL)`.
#' - `syll_cache_info()` devuelve una lista con tamaño y claves.
#'
#' @examples
#' \dontrun{
#'   syll_cache_info()
#'   syll_cache_clear()
#' }
#' @family syllabify-es
#' @name syll-cache
NULL

#' @rdname syll-cache
#' @export
syll_cache_clear <- function() {
  .cache_clear()
  invisible(NULL)
}

#' @rdname syll-cache
#' @export
syll_cache_info <- function() {
  keys <- ls(envir = .syll_cache)
  list(
    size = length(keys),
    keys = keys
  )
}
