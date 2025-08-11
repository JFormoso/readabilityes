# ---------- Internos ----------

.syll_default_options <- function() {
  list(
    "syll.es.valid_letters" = .valid_letters,
    "syll.es.v_fuerte"      = .v_fuerte,
    "syll.es.v_debil"       = .v_debil,
    "syll.es.v_tilde"       = .v_tilde,
    "syll.es.clusters_lr"   = .clusters_lr,
    "syll.es.cache_enabled" = TRUE
  )
}

.keymap <- list(
  valid_letters = "syll.es.valid_letters",
  v_fuerte      = "syll.es.v_fuerte",
  v_debil       = "syll.es.v_debil",
  v_tilde       = "syll.es.v_tilde",
  clusters_lr   = "syll.es.clusters_lr",
  cache_enabled = "syll.es.cache_enabled"
)

.resolve_key <- function(name) {
  if (startsWith(name, "syll.es.")) return(name)
  out <- .keymap[[name]]
  if (is.null(out)) stop(sprintf("Opci\u00f3n desconocida: `%s`.", name), call. = FALSE)
  out
}

.chk_char_1 <- function(x, what) {
  if (!is.character(x) || length(x) != 1L) stop(sprintf("`%s` debe ser character(1).", what), call. = FALSE)
}

.chk_char_vec <- function(x, what) {
  if (!is.character(x)) stop(sprintf("`%s` debe ser un vector de character.", what), call. = FALSE)
}

.chk_logical_1 <- function(x, what) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) stop(sprintf("`%s` debe ser logical(1) no NA.", what), call. = FALSE)
}

.ensure_subset_letters <- function(x, what) {
  if (is.character(x) && length(x) == 1L) {
    x <- strsplit(x, "", fixed = TRUE)[[1]]
  }
  bad <- setdiff(unique(x), strsplit(.valid_letters, "", fixed = TRUE)[[1]])
  if (length(bad)) stop(sprintf("`%s` contiene s\u00edmbolos no permitidos: %s", what, paste(bad, collapse = " ")), call. = FALSE)
}

.ensure_clusters <- function(x) {
  if (any(nchar(x) != 2L)) {
    stop("Todos los `clusters_lr` deben ser bigramas (longitud 2).", call. = FALSE)
  }
  letters <- strsplit(.valid_letters, "", fixed = TRUE)[[1]]
  bad <- vapply(
    x,
    function(cl) any(!strsplit(cl, "", fixed = TRUE)[[1]] %in% letters),
    logical(1)
  )
  if (any(bad)) {
    stop("`clusters_lr` contiene letras fuera del alfabeto válido.", call. = FALSE)
  }

  # Prohibir vocales (fuertes, débiles y tildadas)
  vowels <- unique(c(.v_fuerte, .v_debil, .v_tilde))
  has_vowel <- vapply(
    x,
    function(cl) any(strsplit(cl, "", fixed = TRUE)[[1]] %in% vowels),
    logical(1)
  )
  if (any(has_vowel)) {
    stop("`clusters_lr` no debe contener vocales.", call. = FALSE)
  }

  # Debe terminar en l o r
  if (any(!grepl("[lr]$", x))) {
    stop("`clusters_lr` debe terminar en 'l' o 'r'.", call. = FALSE)
  }
}


# ---------- API pública ----------

#' Configuración del paquete (opciones de sílabas para ES)
#'
#' Consulta y modificación de opciones de silabificación.
#' @keywords internal
#' @family syllabify-es
#' @name syll-config
NULL

#' @rdname syll-config
#' @export
syll_config <- function() {
  dflt <- .syll_default_options()
  cur  <- options()
  out <- lapply(names(dflt), function(k) if (!is.null(cur[[k]])) cur[[k]] else dflt[[k]])
  names(out) <- sub("^syll\\.es\\.", "", names(dflt))
  out
}

#' @rdname syll-config
#' @export
syll_get_option <- function(name, default = NULL) {
  key <- .resolve_key(name)
  val <- getOption(key, NULL)
  if (!is.null(val)) return(val)
  if (!is.null(default)) return(default)
  .syll_default_options()[[key]]
}

#' @rdname syll-config
#' @export
syll_set_options <- function(...) {
  args <- list(...)
  if (!length(args)) return(invisible(list()))
  for (nm in names(args)) {
    key <- .resolve_key(nm)
    val <- args[[nm]]
    switch(
      key,
      "syll.es.valid_letters" = { .chk_char_1(val, "valid_letters"); .ensure_subset_letters(val, "valid_letters") },
      "syll.es.v_fuerte"      = { .chk_char_vec(val, "v_fuerte");    .ensure_subset_letters(val, "v_fuerte") },
      "syll.es.v_debil"       = { .chk_char_vec(val, "v_debil");     .ensure_subset_letters(val, "v_debil") },
      "syll.es.v_tilde"       = { .chk_char_vec(val, "v_tilde");     .ensure_subset_letters(val, "v_tilde") },
      "syll.es.clusters_lr"   = { .chk_char_vec(val, "clusters_lr"); .ensure_clusters(val) },
      "syll.es.cache_enabled" = { .chk_logical_1(val, "cache_enabled") },
      stop("Validaci\u00f3n no implementada para: ", key, call. = FALSE)
    )
  }
  kv <- stats::setNames(unname(args), vapply(names(args), .resolve_key, character(1)))
  old <- options(kv)
  invisible(old)
}

#' @rdname syll-config
#' @export
syll_reset_options <- function(which = "all") {
  dflt <- .syll_default_options()
  if (identical(which, "all")) {
    options(dflt)
    return(invisible(dflt))
  }
  keys <- vapply(which, .resolve_key, character(1))
  set  <- dflt[keys]
  options(set)
  invisible(set)
}
