#' Internos para segmentación silábica en español
#'
#' Conjunto de reglas, constantes y expresiones regulares precompiladas
#' empleadas por el motor de segmentación silábica en español.
#'
#' @section Contenido:
#' - `.valid_letters`: letras válidas en minúsculas (incluye tildes, ñ y diéresis).
#' - `.v_fuerte`: vocales fuertes.
#' - `.v_debil`: vocales débiles (incluye `ï`/`ü` para marcar no tónicas).
#' - `.v_tilde`: vocales acentuadas.
#' - `.clusters_lr`: grupos consonánticos que no se separan (combinaciones con l o r, p. ej., `bl`, `br`, `cl`, `cr`, ...).
#' - `.rx_valid_letters`, `.rx_vowel`, `.rx_strong`, `.rx_weak`: regex precompiladas.
#' - `.syll_cache`: entorno para cachear segmentaciones.
#'
#' @details Estos objetos se inicializan en `.onLoad()` y no deben modificarse
#' fuera de ese hook.
#'
#' @keywords internal
#' @family syllabify-es
#' @seealso `.onLoad()`
#' @name syllabify-internals
NULL

# Letras válidas
#' @rdname syllabify-internals
.valid_letters <- "abcdefghijklmn\u00f1opqrstuvwxyz\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00ef"


# Vocales fuertes / débiles / con tilde
#' @rdname syllabify-internals
.v_fuerte <- c("a","\u00e1","e","\u00e9","o","\u00f3")
#' @rdname syllabify-internals
.v_debil  <- c("i","u","\u00ef","\u00fc")
#' @rdname syllabify-internals
.v_tilde  <- c("\u00e1","\u00e9","\u00ed","\u00f3","\u00fa")

# Grupos consonánticos que no se separan
#' @rdname syllabify-internals
.clusters_lr <- c("bl","br","cl","cr","dr","fl","fr","gl","gr","pl","pr","tr")

# Regex precompiladas (inicializadas en .onLoad)
#' @rdname syllabify-internals
.rx_valid_letters <- NULL
#' @rdname syllabify-internals
.rx_vowel <- NULL
#' @rdname syllabify-internals
.rx_strong <- NULL
#' @rdname syllabify-internals
.rx_weak <- NULL

# Caché para segmentación (creado en .onLoad)
#' @rdname syllabify-internals
.syll_cache <- NULL

