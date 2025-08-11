# syll_core.R — Lógica de silabificación (núcleo)
# -------------------------------------------------------------------
# Nota: usa constantes y regex inicializadas en .onLoad()
# - Vocales fuertes/débiles: .v_fuerte, .v_debil
# - Tildes: .v_tilde
# - Clústeres consonánticos que no se separan: .clusters_lr
# - Caché: .cache_get(), .cache_set()
# - Opciones: syll_get_option("cache_enabled")

# ---------- Helpers internos (no exportar) ----------

# ¿Es vocal (incluye tildes y diéresis)?
.is_vowel <- function(ch) {
  # Usamos las listas internas para decidir
  ch %in% c(.v_fuerte, .v_debil, .v_tilde)
}

# ¿Es vocal fuerte?
# á,é,ó consideradas fuertes
.is_strong <- function(ch) {
  ch %in% .v_fuerte || ch %in% setdiff(.v_tilde, c("\u00ed", "\u00fa"))
}

# ¿Es vocal débil (i/u, ï/ü sin tilde)?
.is_weak <- function(ch) {
  ch %in% .v_debil
}

# ¿La pareja de vocales forma diptongo?
.forms_diphthong_pair <- function(v1, v2) {
  # Reglas aproximadas:
  # - fuerte + fuerte => NO (hiato)
  # - fuerte + débil => SÍ (diptongo)
  # - débil + fuerte => SÍ (diptongo)
  # - débil + débil  => SÍ (diptongo)
  # Consideración de tilde: í/ú fuerzan hiato (se tratan como "fuertes" especiales)
  is_v1_weak <- .is_weak(v1)
  is_v2_weak <- .is_weak(v2)
  is_v1_str  <- .is_strong(v1) || v1 %in% c("\u00ed", "\u00fa")
  is_v2_str  <- .is_strong(v2) || v2 %in% c("\u00ed", "\u00fa")

  # Si ambos "fuertes" (o weak acentuada) => hiato
  if (is_v1_str && is_v2_str) return(FALSE)

  # Si alguna es fuerte y la otra débil sin tilde => diptongo
  if ((is_v1_str && is_v2_weak) || (is_v2_str && is_v1_weak)) return(TRUE)

  # débil + débil => diptongo
  if (is_v1_weak && is_v2_weak) return(TRUE)

  FALSE
}

# Dado un vector de caracteres, compacta núcleos vocálicos en
# di/triptongos y devuelve data.frame con inicios y fines de cada núcleo.
.find_nuclei <- function(chars) {
  n <- length(chars)
  i <- 1L
  starts <- integer()
  ends   <- integer()

  while (i <= n) {
    if (.is_vowel(chars[i])) {
      start <- i
      end   <- i

      # Intentamos extender a diptongo/triptongo
      # Miramos adyacentes vocálicos inmediatos
      if (i + 1L <= n && .is_vowel(chars[i + 1L]) &&
          .forms_diphthong_pair(chars[i], chars[i + 1L])) {
        end <- i + 1L
        # ¿Triptongo? patrón aprox: débil + fuerte + débil (sin tilde en débiles)
        if (i + 2L <= n && .is_vowel(chars[i + 2L])) {
          v1 <- chars[i]; v2 <- chars[i + 1L]; v3 <- chars[i + 2L]
          if (.is_weak(v1) && (.is_strong(v2) || v2 %in% c("\u00e1","\u00e9","\u00f3")) && .is_weak(v3)) {
            end <- i + 2L
          }
        }
      }

      starts <- c(starts, start)
      ends   <- c(ends, end)
      i <- end + 1L
    } else {
      i <- i + 1L
    }
  }

  data.frame(start = starts, end = ends)
}

# Decide el corte de sílaba entre dos núcleos vocálicos consecutivos
# según el grupo consonántico intermedio.
# Devuelve índice (posición final) de la sílaba actual (boundary).
.choose_boundary <- function(chars, left_end, right_start) {
  # Segmento intermedio (consonantes entre núcleos)
  inter_from <- left_end + 1L
  inter_to   <- right_start - 1L
  if (inter_from > inter_to) return(left_end) # V V adyacentes (ya resuelto por núcleos)

  group <- chars[inter_from:inter_to]
  m <- length(group)

  if (m == 1L) {
    # VCV => V-CV (consonante al onset siguiente)
    return(left_end)
  }

  if (m == 2L) {
    # VCCV => VC-CV, excepto clúster lr que no se separa (V-CCV)
    cluster <- paste0(group[1L], group[2L])
    if (cluster %in% .clusters_lr) {
      return(left_end)    # V-CCV
    } else {
      return(left_end + 1L) # VC-CV
    }
  }

  if (m == 3L) {
    # VCCCV => en general VC-CCV si las dos últimas forman clúster;
    # si no, VCC-CV
    last2 <- paste0(group[2L], group[3L])
    if (last2 %in% .clusters_lr) {
      return(left_end + 1L)  # VC-CCV
    } else {
      return(left_end + 2L)  # VCC-CV
    }
  }

  # m >= 4 : regla general -> dejar las últimas dos para el onset siguiente
  return(left_end + (m - 2L))
}

# Núcleo principal de silabificación en una palabra ya limpia
.syllabify_word <- function(word) {
  # Caso trivial
  if (is.na(word) || !nzchar(word)) return(NA_character_)
  chars <- strsplit(word, "", fixed = TRUE)[[1L]]
  n <- length(chars)

  # Si no hay vocales, todo como una sola "sílabla"
  if (!any(.is_vowel(chars))) return(word)

  nuclei <- .find_nuclei(chars)
  k <- nrow(nuclei)
  if (k == 0L) return(word)

  # Construimos límites
  syll_starts <- integer(k)
  syll_ends   <- integer(k)

  # Primera sílaba inicia en 1 (incluye consonantes iniciales, si las hay)
  syll_starts[1L] <- 1L
  syll_ends[k]    <- n

  # Cortes entre núcleos
  for (i in seq_len(k - 1L)) {
    left_end  <- nuclei$end[i]
    right_start <- nuclei$start[i + 1L]
    boundary <- .choose_boundary(chars, left_end, right_start)
    syll_ends[i]     <- boundary
    syll_starts[i+1] <- boundary + 1L
  }

  # Asegurar que la última sílaba termina al final si no fue seteada
  if (is.na(syll_ends[k])) syll_ends[k] <- n

  # Extraer sílabas
  out <- character(k)
  for (i in seq_len(k)) {
    out[i] <- paste0(chars[syll_starts[i]:syll_ends[i]], collapse = "")
  }
  out
}

# ---------- API pública ----------

#' Divide una palabra en sílabas (ES)
#'
#' Implementa una silabificación aproximada para español, respetando
#' clústeres consonánticos típicos (C+l/r) y reglas básicas de diptongos/
#' triptongos (í/ú fuerzan hiato). La implementación es determinista y
#' suficientemente precisa para conteo y segmentación general.
#'
#' @param word Cadena (longitud 1). Se limpiará con [clean_word()] antes
#'   de segmentar. Si queda vacía, devuelve `NA`.
#' @return Vector de sílabas en orden, o `NA_character_` si no se pudo segmentar.
#' @examples
#' syll_split("canción")   # "can","ción"
#' syll_split("pingüino")  # "pin","güi","no"
#' syll_split("transporte")# "trans","por","te"
#' @export
#' @family syllabify-es
syll_split <- function(word) {
  w <- clean_word(word)
  if (is.na(w)) return(NA_character_)

  # Caché opcional
  if (isTRUE(syll_get_option("cache_enabled"))) {
    cached <- .cache_get(w)
    if (!is.null(cached)) return(cached)
  }

  out <- .syllabify_word(w)

  if (isTRUE(syll_get_option("cache_enabled")) && !is.na(out[1L])) {
    .cache_set(w, out)
  }
  out
}

#' Cuenta sílabas de una palabra (ES)
#'
#' Envuelve [syll_split()] y devuelve el número de sílabas.
#'
#' @param word Cadena (longitud 1).
#' @return Entero con el número de sílabas o `NA_integer_` si no aplica.
#' @examples
#' syll_count("canción")   # 2
#' syll_count("extraordinario")  # 6 (aprox.)
#' @export
#' @family syllabify-es
syll_count <- function(word) {
  s <- syll_split(word)
  if (length(s) == 1L && is.na(s)) return(NA_integer_)
  length(s)
}

#' Inserta guiones suaves entre sílabas
#'
#' Útil para visualización o exportes tipográficos sencillos.
#'
#' @param word Cadena (longitud 1).
#' @param hyphen Cadena usada como separador (por defecto `"-"`).
#' @return Cadena con separadores, o `NA` si no se pudo segmentar.
#' @examples
#' syll_hyphenate("silabificación")  # "si-la-bi-fi-ca-ción" (aprox.)
#' @export
#' @family syllabify-es
syll_hyphenate <- function(word, hyphen = "-") {
  s <- syll_split(word)
  if (length(s) == 1L && is.na(s)) return(NA_character_)
  paste(s, collapse = hyphen)
}
