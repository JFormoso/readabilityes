#' Registro central de recursos por idioma
#' @keywords internal
.lang_registry <- NULL   # se construye en .onLoad(), ver zzz.R

#' Obtiene la entrada del registro para un idioma
#' @keywords internal
.lang_get <- function(lang) {
  entry <- .lang_registry[[lang]]
  if (is.null(entry)) {
    stop(sprintf("Idioma no soportado: '%s'. Usar uno de: %s.",
                 lang, paste(names(.lang_registry), collapse = ", ")), call. = FALSE)
  }
  entry
}
