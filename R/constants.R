#' Internos para segmentación silábica
#'
#' Conjunto de reglas, constantes y expresiones regulares precompiladas
#' empleadas por el motor de segmentación silábica en español y catalán.
#'
#' @section Contenido (español):
#' - `.valid_letters`: letras válidas en minúsculas (incluye tildes, ñ y diéresis).
#' - `.v_fuerte`: vocales fuertes.
#' - `.v_debil`: vocales débiles (incluye `ï`/`ü` para marcar no tónicas).
#' - `.v_tilde`: vocales acentuadas.
#' - `.clusters_lr`: grupos consonánticos que no se separan (combinaciones con l o r).
#'
#' @section Contenido (catalán):
#' - `.valid_letters_ca`: letras válidas en minúsculas para catalán.
#' - `.v_fuerte_ca`: vocales fuertes en catalán (a, e, o con y sin acento/grave).
#' - `.v_debil_ca`: vocales débiles en catalán (i, u sin acento; ï, ü con diéresis).
#' - `.v_tilde_ca`: vocales acentuadas en catalán (à, è, é, í, ï, ò, ó, ú, ü).
#' - `.digraphs_ca`: dígrafos inseparables propios del catalán (ll, ny, rr, ss, gu, qu).
#' - `.clusters_ca`: grupos consonánticos inseparables en catalán (bl, br, cl, cr, ...).
#'
#' @details Estos objetos se inicializan en `.onLoad()` y no deben modificarse
#' fuera de ese hook.
#'
#' @keywords internal
#' @name syllabify-internals
NULL


# Español ---------------------


# Letras válidas
#' @rdname syllabify-internals
.valid_letters <- "abcdefghijklmn\u00f1opqrstuvwxyz\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00ef"

# Vocales fuertes / débiles / con tilde
#' @rdname syllabify-internals
.v_fuerte <- c("a", "\u00e1", "e", "\u00e9", "o", "\u00f3")
#' @rdname syllabify-internals
.v_debil  <- c("i", "u", "\u00ef", "\u00fc")
#' @rdname syllabify-internals
.v_tilde  <- c("\u00e1", "\u00e9", "\u00ed", "\u00f3", "\u00fa")

# Grupos consonánticos que no se separan (español)
#' @rdname syllabify-internals
.clusters_lr <- c("bl", "br", "cl", "cr", "dr", "fl", "fr", "gl", "gr", "pl", "pr", "tr")


# Catalán --------------------------


# Letras válidas en catalán (extiende el español con à, è, ò, ï, ü, ç, ·)
# Nota: la ela geminada (l·l) se maneja como dígrafo; el punto medio · se
# preserva para detectar la separación l·l → l-l en silabificación.
#' @rdname syllabify-internals
.valid_letters_ca <- paste0(
  .valid_letters,
  "\u00e0",   # à
  "\u00e8",   # è
  "\u00f2",   # ò
  "\u00ef",   # ï  (ya en español, se repite por claridad)
  "\u00fc",   # ü  (ya en español, se repite por claridad)
  "\u00e7",   # ç  (ce trencada — p.ej. traça, força)
  "\u00b7"    # ·  (punt volat — marca ela geminada l·l)
)

# Vocales fuertes en catalán: a, e, o (tónicas y átonas, con todas sus tildes)
#' @rdname syllabify-internals
.v_fuerte_ca <- c(
  "a", "\u00e0",          # a, à
  "e", "\u00e8", "\u00e9", # e, è, é
  "o", "\u00f2", "\u00f3"  # o, ò, ó
)

# Vocales débiles en catalán: i, u (sin acento ni diéresis — forman diptongos)
#' @rdname syllabify-internals
.v_debil_ca <- c("i", "u")

# Vocales acentuadas en catalán (fuerzan hiato cuando están en i/u)
#' @rdname syllabify-internals
.v_tilde_ca <- c(
  "\u00e0",             # à
  "\u00e8", "\u00e9",   # è, é
  "\u00ed",             # í
  "\u00ef",             # ï  (diéresis, también fuerza hiato)
  "\u00f2", "\u00f3",   # ò, ó
  "\u00fa",             # ú
  "\u00fc"              # ü  (diéresis, también fuerza hiato)
)

# Dígrafos inseparables en catalán
# Cada elemento es un dígrafo que se trata como una sola consonante
#' @rdname syllabify-internals
.digraphs_ca <- c(
  "ll",  # ela geminada (fonema lateral palatal)
  "ny",  # eñe catalana  (fonema nasal palatal)
  "rr",  # erra doble     (vibrante múltiple)
  "ss",  # esse doble     (sibilante sorda)
  "tg",  # africada sonora ante e/i
  "tx"   # africada sorda
  # gu y qu se manejan por separado en el preprocesamiento
  # porque dependen de la vocal siguiente
)

# Grupos consonánticos inseparables en catalán
# (no se separan porque pueden iniciar sílaba)
#' @rdname syllabify-internals
.clusters_ca <- c(
  # Oclusiva/fricativa + líquida
  "bl", "br",
  "cl", "cr",
  "dr",
  "fl", "fr",
  "gl", "gr",
  "pl", "pr",
  "tr"
)


# Diptongos catalanes
.diphthongs_ca <- c(
  "ai","ei","oi",
  "au","eu","ou",
  "iu","ui"
)

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
