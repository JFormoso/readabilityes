# ---------- Internos ----------

.syll_default_options <- function() {
  es <- .lang_get("es")
  ca <- .lang_get("ca")
  list(
    "syll.es.valid_letters" = es$valid_letters,
    "syll.es.v_fuerte"      = es$v_fuerte,
    "syll.es.v_debil"       = es$v_debil,
    "syll.es.v_tilde"       = es$v_tilde,
    "syll.es.clusters_lr"   = es$clusters,
    "syll.ca.valid_letters" = ca$valid_letters,
    "syll.ca.v_fuerte"      = ca$v_fuerte,
    "syll.ca.v_debil"       = ca$v_debil,
    "syll.ca.v_tilde"       = ca$v_tilde,
    "syll.ca.clusters"      = ca$clusters,
    "syll.cache_enabled"    = TRUE,          # compartido entre idiomas, ver nota abajo
    "syll.lang"             = "es"           # idioma global por defecto
  )
}

.keymap <- list(
  valid_letters    = "syll.es.valid_letters",
  v_fuerte         = "syll.es.v_fuerte",
  v_debil          = "syll.es.v_debil",
  v_tilde          = "syll.es.v_tilde",
  clusters_lr      = "syll.es.clusters_lr",
  valid_letters_ca = "syll.ca.valid_letters",
  v_fuerte_ca      = "syll.ca.v_fuerte",
  v_debil_ca       = "syll.ca.v_debil",
  v_tilde_ca       = "syll.ca.v_tilde",
  clusters_ca      = "syll.ca.clusters",
  cache_enabled    = "syll.cache_enabled",
  lang             = "syll.lang"
)

.resolve_key <- function(name) {
  # Claves completamente calificadas se pasan tal cual
  if (startsWith(name, "syll.")) return(name)
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

# Generalizada: acepta un alfabeto distinto para poder validar catalán
# sin rechazar sus propias letras (à, è, ò, ç, ï, ü, ·) como "inválidas".
.ensure_subset_letters <- function(x, what, valid_alphabet = .valid_letters) {
  if (is.character(x) && length(x) == 1L) {
    x <- strsplit(x, "", fixed = TRUE)[[1]]
  }
  bad <- setdiff(unique(x), strsplit(valid_alphabet, "", fixed = TRUE)[[1]])
  if (length(bad)) stop(sprintf("`%s` contiene s\u00edmbolos no permitidos: %s", what, paste(bad, collapse = " ")), call. = FALSE)
}

# Generalizada: mismo motivo, ahora también para las vocales a excluir.
.ensure_clusters <- function(x, valid_alphabet = .valid_letters,
                             vowels = unique(c(.v_fuerte, .v_debil, .v_tilde))) {
  if (any(nchar(x) != 2L)) {
    stop("Todos los `clusters` deben ser bigramas (longitud 2).", call. = FALSE)
  }
  letters <- strsplit(valid_alphabet, "", fixed = TRUE)[[1]]
  bad <- vapply(
    x,
    function(cl) any(!strsplit(cl, "", fixed = TRUE)[[1]] %in% letters),
    logical(1)
  )
  if (any(bad)) {
    stop("`clusters` contiene letras fuera del alfabeto v\u00e1lido.", call. = FALSE)
  }

  has_vowel <- vapply(
    x,
    function(cl) any(strsplit(cl, "", fixed = TRUE)[[1]] %in% vowels),
    logical(1)
  )
  if (any(has_vowel)) {
    stop("`clusters` no debe contener vocales.", call. = FALSE)
  }

  if (any(!grepl("[lr]$", x))) {
    stop("`clusters` debe terminar en 'l' o 'r'.", call. = FALSE)
  }
}

.chk_lang <- function(x) {
  valid <- c("es", "ca")
  if (!is.character(x)) {
    stop(sprintf("`lang` debe ser character(1); recibido: %s.", class(x)[1L]), call. = FALSE)
  }
  if (length(x) != 1L) {
    stop(sprintf("`lang` debe ser character(1) (longitud 1); recibido longitud %d.", length(x)), call. = FALSE)
  }
  if (!x %in% valid) {
    stop(sprintf("`lang` debe ser uno de: %s.", paste(valid, collapse = ", ")), call. = FALSE)
  }
}


# ---------- API pública ----------

#' Configuración del paquete
#'
#' Consulta y modificación de opciones de silabificación para español y
#' catalán, incluyendo el idioma global (`lang`).
#'
#' @examples
#' syll_get_option("lang")        # "es"
#' syll_set_options(lang = "ca")  # cambia a catalán globalmente
#' syll_reset_options()           # restaura todo a los valores por defecto
#'
#' @keywords internal
#' @name syll-config
NULL

#' @rdname syll-config
#' @export
syll_config <- function() {
  dflt <- .syll_default_options()
  cur  <- options()
  out <- lapply(names(dflt), function(k) if (!is.null(cur[[k]])) cur[[k]] else dflt[[k]])

  rev_map <- stats::setNames(names(.keymap), unlist(.keymap))
  names(out) <- unname(rev_map[names(dflt)])

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
      "syll.ca.valid_letters" = { .chk_char_1(val, "valid_letters_ca"); .ensure_subset_letters(val, "valid_letters_ca", .valid_letters_ca) },
      "syll.ca.v_fuerte"      = { .chk_char_vec(val, "v_fuerte_ca");    .ensure_subset_letters(val, "v_fuerte_ca", .valid_letters_ca) },
      "syll.ca.v_debil"       = { .chk_char_vec(val, "v_debil_ca");     .ensure_subset_letters(val, "v_debil_ca", .valid_letters_ca) },
      "syll.ca.v_tilde"       = { .chk_char_vec(val, "v_tilde_ca");     .ensure_subset_letters(val, "v_tilde_ca", .valid_letters_ca) },
      "syll.ca.clusters"      = { .chk_char_vec(val, "clusters_ca");    .ensure_clusters(val, .valid_letters_ca, unique(c(.v_fuerte_ca, .v_debil_ca, .v_tilde_ca))) },
      "syll.cache_enabled"    = { .chk_logical_1(val, "cache_enabled") },
      "syll.lang"             = { .chk_lang(val) },
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
