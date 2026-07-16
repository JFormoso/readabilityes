# syll_core.R: Lógica de silabificación (núcleo)
# -------------------------------------------------------------------
# Nota: usa constantes y regex inicializadas en .onLoad()
# - Vocales fuertes/débiles: .v_fuerte, .v_debil
# - Tildes: .v_tilde
# - Clústeres consonánticos que no se separan: .clusters_lr
# - Caché: .cache_get(), .cache_set()
# - Opciones: syll_get_option("cache_enabled"), syll_get_option("lang")
#
# Idiomas soportados:
# - "es" : español (lógica original)
# - "ca" : catalán  (lógica nueva — ver sección CA más abajo)

# HELPERS COMPARTIDOS -------------

# ¿Es vocal (incluye tildes y diéresis)?
.is_vowel <- function(ch) {
  ch %in% c(.v_fuerte, .v_debil, .v_tilde)
}

# ¿Es vocal fuerte? (a, e, o — con o sin tilde, excepto í/ú que fuerzan hiato)
.is_strong <- function(ch) {
  ch %in% .v_fuerte || ch %in% setdiff(.v_tilde, c("\u00ed", "\u00fa"))
}

# ¿Es vocal débil? (i/u sin tilde ni diéresis)
.is_weak <- function(ch) {
  ch %in% .v_debil
}

# ESPAÑOL -----------------------

# ¿La pareja de vocales forma diptongo en español?
.forms_diphthong_pair_es <- function(v1, v2) {
  is_v1_weak <- .is_weak(v1)
  is_v2_weak <- .is_weak(v2)
  is_v1_str  <- .is_strong(v1) || v1 %in% c("\u00ed", "\u00fa")
  is_v2_str  <- .is_strong(v2) || v2 %in% c("\u00ed", "\u00fa")

  if (is_v1_str && is_v2_str) return(FALSE)
  if ((is_v1_str && is_v2_weak) || (is_v2_str && is_v1_weak)) return(TRUE)
  if (is_v1_weak && is_v2_weak) return(TRUE)
  FALSE
}

# Compacta núcleos vocálicos en di/triptongos (español)
.find_nuclei_es <- function(chars) {
  n <- length(chars)
  i <- 1L
  starts <- integer()
  ends   <- integer()

  while (i <= n) {
    if (.is_vowel(chars[i])) {
      start <- i
      end   <- i

      if (i + 1L <= n && .is_vowel(chars[i + 1L]) &&
          .forms_diphthong_pair_es(chars[i], chars[i + 1L])) {
        end <- i + 1L
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

# Decide el corte de sílaba entre dos núcleos (español)
.choose_boundary_es <- function(chars, left_end, right_start) {
  inter_from <- left_end + 1L
  inter_to   <- right_start - 1L
  if (inter_from > inter_to) return(left_end)

  group <- chars[inter_from:inter_to]
  m <- length(group)

  if (m == 1L) return(left_end)

  if (m == 2L) {
    cluster <- paste0(group[1L], group[2L])
    if (cluster %in% .clusters_lr) return(left_end)
    return(left_end + 1L)
  }

  if (m == 3L) {
    last2 <- paste0(group[2L], group[3L])
    if (last2 %in% .clusters_lr) return(left_end + 1L)
    return(left_end + 2L)
  }

  return(left_end + (m - 2L))
}

# Silabifica una palabra en español
.syllabify_word_es <- function(word) {
  if (is.na(word) || !nzchar(word)) return(NA_character_)
  chars <- strsplit(word, "", fixed = TRUE)[[1L]]
  n <- length(chars)

  if (!any(.is_vowel(chars))) return(word)

  nuclei <- .find_nuclei_es(chars)
  k <- nrow(nuclei)
  if (k == 0L) return(word)

  syll_starts <- integer(k)
  syll_ends   <- integer(k)
  syll_starts[1L] <- 1L
  syll_ends[k]    <- n

  for (i in seq_len(k - 1L)) {
    boundary        <- .choose_boundary_es(chars, nuclei$end[i], nuclei$start[i + 1L])
    syll_ends[i]    <- boundary
    syll_starts[i + 1L] <- boundary + 1L
  }

  if (is.na(syll_ends[k])) syll_ends[k] <- n

  out <- character(k)
  for (i in seq_len(k)) {
    out[i] <- paste0(chars[syll_starts[i]:syll_ends[i]], collapse = "")
  }
  out
}

# CATALÁN --------------------------------

# Vocales catalanas (todas las formas)
.vowels_ca <- c(
  "a", "\u00e0",                      # a, à
  "e", "\u00e8", "\u00e9",            # e, è, é
  "i", "\u00ed", "\u00ef",            # i, í, ï
  "o", "\u00f2", "\u00f3",            # o, ò, ó
  "u", "\u00fa", "\u00fc"             # u, ú, ü
)

.is_vowel_ca <- function(ch) ch %in% .vowels_ca

# Vocales que siempre forman núcleo propio (fuerzan hiato con adyacentes)
# i/u con tilde o diéresis son tónicas → hiato
.is_strong_ca <- function(ch) ch %in% .v_fuerte_ca
.is_accent_ca <- function(ch) ch %in% c("\u00ed", "\u00ef", "\u00fa", "\u00fc")  # í ï ú ü

# ¿La pareja de vocales catalanas forma diptongo?
#
# Regla fonológica catalana:
#   - i/u CON tilde o diéresis (í, ï, ú, ü) → siempre hiato.
#   - fuerte + débil (a+i, e+u, etc.) → hiato  (ri-a-lla, du-es, di-a-ri)
#   - débil  + fuerte (i+a, u+e, etc.) → hiato  (cru-el, su-or)
#   - fuerte + fuerte → hiato siempre
#   - débil  + débil  → diptongo solo si el par es reconocido: iu, ui
#     (riu-re, viu, cuina)

.forms_diphthong_ca <- function(v1, v2) {
  # Tilde/diéresis en i/u siempre fuerza hiato
  if (.is_accent_ca(v1) || .is_accent_ca(v2)) return(FALSE)

  s1 <- .is_strong_ca(v1)
  s2 <- .is_strong_ca(v2)
  w1 <- v1 %in% .v_debil_ca
  w2 <- v2 %in% .v_debil_ca

  if (s1 && w2) return(TRUE)    # fuerte+débil → diptongo decreciente (fei-na, sau-na, mai, rei)
  if (w1 && s2) return(FALSE)   # débil+fuerte → hiato en catalán (cru-el, su-or, du-es)
  if (s1 && s2) return(FALSE)   # fuerte+fuerte → hiato
  if (w1 && w2) return(paste0(v1, v2) %in% c("iu", "ui"))  # débil+débil: solo iu/ui

  FALSE
}


# Tokenización en unidades: convierte la cadena en un vector donde cada
# dígrafo catalán (ll, ny, rr, ss, tg, tx, l·l) ocupa UNA posición.
# l·l se marca como "L" (ela geminada, se separa entre sílabas).
# ll se deja como "ll" (dígrafo inseparable).

.tokenize_ca <- function(chars) {
  n <- length(chars)
  tokens <- character(0)
  i <- 1L
  while (i <= n) {
    # l·l  →  token especial "L" (representa separación entre dos /l/)
    if (i + 2L <= n && chars[i] == "l" && chars[i + 1L] == "\u00b7" && chars[i + 2L] == "l") {
      tokens <- c(tokens, "L")
      i <- i + 3L
      next
    }
    # dígrafos inseparables: ll, ny, rr, ss, tg, tx, gu, qu
    if (i + 1L <= n) {
      di <- paste0(chars[i], chars[i + 1L])
      if (di %in% c("ll", "ny", "rr", "ss", "tg", "tx")) {
        tokens <- c(tokens, di)
        i <- i + 2L
        next
      }
      # gu/qu ante e/i → dígrafo (la u es muda)
      if (di %in% c("gu", "qu") && i + 2L <= n && .is_vowel_ca(chars[i + 2L])) {
        tokens <- c(tokens, di)
        i <- i + 2L
        next
      }
    }
    tokens <- c(tokens, chars[i])
    i <- i + 1L
  }
  tokens
}


# Detecta si "ui" en posición `pos_u` del vector de tokens es hiato morfológico.
# Combina dos estrategias en orden:
#   1. Sufijo verbal (-uir y sus formas conjugadas): Opción A
#   2. Heurística posicional (¿la i va seguida de r?): Opción B
# Sufijos verbales de verbos en -uir
.uir_verb_suffixes <- c(
  "ir", "ire",                                    # infinitivo: conduir, conduire
  "ire", "ira", "iras", "irem", "ireu", "iran",   # futuro
  "iria", "iries", "iria", "iriem", "irieu",      # condicional
  "ia", "ies",                                    # imperfecto: conduia
  "int",                                          # gerundio: conduint
  "it", "ida", "its", "ides"                      # participio: conduït
)

.is_uir_verb <- function(word) {
  if (!grepl("ui", word, fixed = TRUE)) return(FALSE)
  any(sapply(.uir_verb_suffixes, function(s) endsWith(word, s)))
}

# Heurística posicional
# Si después de la "i" viene "r" (sola o seguida de vocal/nada) -> probable desinencia -> hiato
# Si la "i" es la última vocal o va seguida de consonante de coda -> diptongo
.ui_is_hiatus_positional <- function(tokens, pos_u) {
  pos_i   <- pos_u + 1L
  n       <- length(tokens)
  if (pos_i >= n) return(FALSE)       # "buit", "ui" al final -> diptongo

  after_i <- tokens[(pos_i + 1L):n]
  after_i <- after_i[nzchar(after_i)]
  if (length(after_i) == 0L) return(FALSE)  # nada después → diptongo

  # "r" inmediatamente después de la "i" -> hiato verbal (con-du-ir, in-flu-i-ré)
  if (after_i[1L] == "r") return(TRUE)

  FALSE
}

# Función combinada: devuelve TRUE si "ui" en pos_u debe tratarse como hiato
.ui_is_hiatus <- function(tokens, pos_u, word) {
  # Primero: sufijo verbal explícito (más confiable)
  if (.is_uir_verb(word)) return(TRUE)
  # Segundo: heurística posicional
  if (.ui_is_hiatus_positional(tokens, pos_u)) return(TRUE)
  FALSE
}


# Encuentra los núcleos vocálicos en el vector de tokens catalanes.
# Devuelve data.frame con columnas start/end (índices en `tokens`).
.find_nuclei_ca <- function(tokens, word = "") {
  n <- length(tokens)
  i <- 1L
  starts <- integer()
  ends   <- integer()

  while (i <= n) {
    if (.is_vowel_ca(tokens[i])) {
      start <- i
      end   <- i

      if (i + 1L <= n && .is_vowel_ca(tokens[i + 1L])) {
        v1 <- tokens[i]
        v2 <- tokens[i + 1L]

        # Antes de formar diptongo v1+v2, verificar si v2 es el inicio
        # de un triptongo (v2+v3+v4). Si lo es, v1 queda como núcleo solo
        # y el triptongo se detectará en la siguiente iteración del while.
        # Ej: creieu → e(3)+i(4)+e(5)+u(6): cuando i=3, v2="i",
        # tokens[4+1]="e", tokens[4+2]="u" → "ieu" → no formar diptongo "ei",
        # dejar e(3) solo. Luego i=4: "ieu" se detecta como triptongo.
        v2_inicia_triptongo <- FALSE
        if (i + 3L <= n &&
            .is_vowel_ca(tokens[i + 2L]) &&
            .is_vowel_ca(tokens[i + 3L])) {
          trip_desde_v2 <- paste0(tokens[i + 1L], tokens[i + 2L], tokens[i + 3L])
          if (trip_desde_v2 %in% c("ieu", "iau", "ueu", "uai", "uei")) {
            v2_inicia_triptongo <- TRUE
          }
        }

        if (!v2_inicia_triptongo) {
          # Intentar triptongo desde la posición actual
          triptongo_detectado <- FALSE
          if (i + 2L <= n && .is_vowel_ca(tokens[i + 2L])) {
            trip3 <- paste0(tokens[i], tokens[i + 1L], tokens[i + 2L])
            if (trip3 %in% c("ieu", "iau", "ueu", "uai", "uei")) {
              end <- i + 2L
              triptongo_detectado <- TRUE
            }
          }

          # Si no hay triptongo, intentar diptongo
          if (!triptongo_detectado) {
            es_diptongo <- if (v1 == "u" && v2 == "i") {
              !.ui_is_hiatus(tokens, i, word)
            } else {
              .forms_diphthong_ca(v1, v2)
            }
            if (es_diptongo) end <- i + 1L
          }
        }
        # Si v2_inicia_triptongo: end queda en i (núcleo solo),
        # y la siguiente iteración arranca en i+1 donde detectará el triptongo
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


# Decide el punto de corte entre dos núcleos en catalán.
# Tiene en cuenta dígrafos inseparables, clusters y ela geminada.
.choose_boundary_ca <- function(tokens, left_end, right_start) {
  inter_from <- left_end + 1L
  inter_to   <- right_start - 1L
  if (inter_from > inter_to) return(left_end)  # vocales adyacentes (hiato ya gestionado)

  group <- tokens[inter_from:inter_to]
  m <- length(group)

  if (m == 1L) {
    tok <- group[1L]
    # rr entre vocales -> se separa (ar-ri): la primera r queda a la izquierda.
    # Como "rr" es un token único, lo dejamos en la sílaba izquierda (left_end+1)
    # y la sílaba derecha empieza en right_start con la segunda r implícita.
    # En la construcción de sílabas, el token "rr" en posición de coda
    # se expande como "r" y el onset de la siguiente sílaba no tiene r extra;
    # para ser fieles a la ortografía, dejamos "rr" en la sílaba izquierda
    # y la derecha arranca desde right_start (que ya tiene su vocal).
    # Resultado: a-"rr" | vocal... -> "ar" | "ri..."
    if (tok == "rr") return(left_end + 1L)  # rr queda en la sílaba izquierda
    # ss entre vocales → mismo comportamiento (as-sa)
    if (tok == "ss") return(left_end + 1L)
    # L (ela geminada sola) → la primera l queda a la izquierda
    if (tok == "L")  return(left_end + 1L)
    # tg, tx, tl, tm, tn: en catalán van en coda, no en onset
    # jut-ges, cot-xe, at-le-ta, set-ma-na, cot-na
    if (tok %in% c("tg", "tx", "tl", "tm", "tn")) return(left_end + 1L)
    # cualquier otra consonante única → va a la derecha (onset)

    return(left_end)
  }

  if (m == 2L) {
    # Ela geminada: "L" representa l·l; la primera l queda en la sílaba izquierda,
    # la segunda va con la derecha. Como "L" es un token único que ocupa una posición,
    # el corte se hace DENTRO del token: left_end + 1 (incluye "L" en la izq).
    if (group[1L] == "L") return(left_end + 1L)    # [L, C] -> L queda izq (con su l izq), C va der
    if (group[2L] == "L") return(left_end + 1L)    # [C, L] -> C queda izq, L va der (con su l der)
    # Cluster inseparable en posición final → todo va a la derecha
    cluster <- paste0(group[1L], group[2L])
    if (cluster %in% .clusters_ca) return(left_end)
    return(left_end + 1L)
  }

  if (m == 3L) {
    # Revisar si los dos últimos forman cluster inseparable
    last2 <- paste0(group[2L], group[3L])
    if (last2 %in% .clusters_ca || group[2L] == "L") return(left_end + 1L)
    return(left_end + 2L)
  }

  # Caso general: dejar máximo 1 consonante al final de la sílaba izquierda
  return(left_end + (m - 2L))
}


# Preprocesa el apóstrofe en catalán:
#   - Apóstrofe INICIAL (l', d', m', s', etc.):
#       El prefijo clítico (sin vocal) se fusiona con la PRIMERA sílaba
#       del núcleo: m'arribi → m'ar-ri-bi (la m va pegada a "ar").
#       Se devuelve como `prefix` para que .syllabify_word_ca lo anteponga
#       a la primera sílaba resultante.
#   - Apóstrofe FINAL (mireu's, dir-li'n, etc.):
#       La parte tras el apóstrofe (sin vocal) se anexa a la última sílaba.
#
# Devuelve lista: list(prefix = string, core = string, suffix = string)
#   prefix: string a anteponer a la primera sílaba (p.ej. "m'"), o ""
#   core:   palabra a silabificar (sin el clítico ni el apóstrofe inicial)
#   suffix: string a anexar a la última sílaba (p.ej. "s"), o ""

.split_apostrophe_ca <- function(word) {
  vowel_pat <- "[aeiou\u00e0\u00e8\u00e9\u00ed\u00ef\u00f2\u00f3\u00fa\u00fc]"

  prefix <- ""
  suffix <- ""

  # --- Apóstrofe inicial ---
  apos_pos <- regexpr("['\u2019]", word, perl = TRUE)
  if (apos_pos > 0L) {
    before <- substr(word, 1L, apos_pos - 1L)
    after  <- substr(word, apos_pos + 1L, nchar(word))

    # El prefijo no contiene vocal → es clítico (m', l', d', s', etc.)
    # Se fusiona con la primera sílaba del núcleo (m'ar-ri-bi)
    if (!grepl(vowel_pat, before, perl = TRUE)) {
      prefix <- paste0(before, "'")  # p.ej. "m'"
      word   <- after
    }
    # Si el prefijo contiene vocal, se silabifica junto con el núcleo
  }

  # --- Apóstrofe final ---
  apos_pos2 <- regexpr("['\u2019]", word, perl = TRUE)
  if (apos_pos2 > 0L) {
    core_part <- substr(word, 1L, apos_pos2 - 1L)
    suf_part  <- substr(word, apos_pos2 + 1L, nchar(word))
    # Sufijo sin vocal → se anexa a la última sílaba (mireu's → mi-reus)
    if (!grepl(vowel_pat, suf_part, perl = TRUE)) {
      suffix <- suf_part
      word   <- core_part
    }
  }

  list(prefix = prefix, core = word, suffix = suffix)
}

# Silabifica una palabra en catalán (lógica completa)
.syllabify_word_ca <- function(word) {
  if (is.na(word) || !nzchar(word)) return(NA_character_)

  # Guión: silabificar cada parte por separado
  if (grepl("-", word, fixed = TRUE)) {
    parts <- strsplit(word, "-", fixed = TRUE)[[1L]]
    parts <- parts[nzchar(parts)]
    if (!length(parts)) return(NA_character_)
    return(unlist(lapply(parts, .syllabify_word_ca)))
  }

  # Gestionar apóstrofes
  apos <- .split_apostrophe_ca(word)
  prefix <- apos$prefix   # string a anteponer a la 1ª sílaba (p.ej. "m'"), o ""
  word   <- apos$core     # núcleo a silabificar
  suffix <- apos$suffix   # sufijo a anexar a la última sílaba (p.ej. "s"), o ""

  if (!nzchar(word)) {
    out <- if (nzchar(prefix)) prefix else NA_character_
    return(out)
  }

  # Tokenizar (gestiona dígrafos y l·l)
  # Normalizar el punt volat (U+00B7) antes de partir en chars,
  # ya que strsplit puede representarlo como secuencia de bytes
  # en lugar de un solo carácter, rompiendo la detección de l·l.
  word <- gsub("\u00b7", "\u00b7", word, fixed = TRUE)
  chars  <- strsplit(word, "", fixed = TRUE, useBytes = FALSE)[[1L]]
  tokens <- .tokenize_ca(chars)
  n      <- length(tokens)

  # Si no hay vocales, devolver como está
  if (!any(.is_vowel_ca(tokens))) {
    out <- if (nzchar(prefix)) paste0(prefix, word) else word
    if (nzchar(suffix)) out <- paste0(out, suffix)
    return(out)
  }

  # Encontrar núcleos vocálicos
  nuclei <- .find_nuclei_ca(tokens, word = word)
  k <- nrow(nuclei)
  if (k == 0L) {
    out <- if (nzchar(prefix)) paste0(prefix, word) else word
    if (nzchar(suffix)) out <- paste0(out, suffix)
    return(out)
  }

  # Calcular límites de sílaba
  syll_starts <- integer(k)
  syll_ends   <- integer(k)
  syll_starts[1L] <- 1L
  syll_ends[k]    <- n

  for (i in seq_len(k - 1L)) {
    boundary            <- .choose_boundary_ca(tokens, nuclei$end[i], nuclei$start[i + 1L])
    syll_ends[i]        <- boundary
    syll_starts[i + 1L] <- boundary + 1L
  }

  # Construir sílabas desde tokens.
  # El token "L" (ela geminada l·l) se expande a "l" en cada sílaba en que cae:
  # la sílaba izquierda recibe una "l" y la sílaba derecha recibe la otra "l".
  # .choose_boundary_ca coloca "L" en la sílaba izquierda cuando está solo
  # (m==1) y en la izquierda también cuando m==2 con L en pos 1.
  # En ambos casos gsub("^L$", "l", ...) da la l correcta para esa sílaba.
  out <- character(k)
  # Rastrear si la sílaba anterior cedió una consonante extra al onset actual
  pending_onset <- ""
  for (i in seq_len(k)) {
    toks <- tokens[syll_starts[i]:syll_ends[i]]

    expanded <- character(length(toks))
    next_pending <- ""

    for (j in seq_along(toks)) {
      tok <- toks[j]
      is_last <- (j == length(toks))
      if (tok == "L") {
        expanded[j] <- "l"
        if (is_last && i < k) next_pending <- "l"   # ← fix Bug 4: l·l genera pending
      } else if (tok == "rr" && is_last && i < k) {
        expanded[j] <- "r"
        next_pending <- "r"
      } else if (tok == "ss" && is_last && i < k) {
        expanded[j] <- "s"
        next_pending <- "s"
      } else if (tok == "tg" && is_last && i < k) {  # ← fix Bug 1a: jutges
        expanded[j] <- "t"
        next_pending <- "g"
      } else if (tok == "tx" && is_last && i < k) {  # ← fix Bug 1b: cotxe
        expanded[j] <- "t"
        next_pending <- "x"
      } else {
        expanded[j] <- tok
      }
    }
    syll_str <- paste0(expanded, collapse = "")
    # Anteponer consonante pendiente del dígrafo anterior (segunda mitad de rr/ss)
    if (nzchar(pending_onset)) syll_str <- paste0(pending_onset, syll_str)
    out[i] <- syll_str
    pending_onset <- next_pending
  }

  # Anexar sufijo a la última sílaba
  if (nzchar(suffix)) out[k] <- paste0(out[k], suffix)

  # Fusionar prefijo clítico con la primera sílaba (m'ar-ri-bi)
  if (nzchar(prefix)) out[1L] <- paste0(prefix, out[1L])

  out
}

# elige la rama según `lang`


.syllabify_word <- function(word, lang = "es") {
  switch(lang,
         "ca" = .syllabify_word_ca(word),
         "es" = .syllabify_word_es(word),
         stop(sprintf("Idioma no soportado: '%s'. Usar 'es' o 'ca'.", lang), call. = FALSE)
  )
}

# API PÚBLICA --------------

#' Divide una palabra en sílabas
#'
#' Implementa una silabificación aproximada para español (`"es"`) y catalán
#' (`"ca"`), respetando clústeres consonánticos, diptongos, triptongos e hiatos.
#'
#' @param word Cadena (longitud 1). Se limpiará con [clean_word()] antes de
#'   segmentar. Si queda vacía, devuelve `NA`.
#' @param lang Código de idioma: `"es"` (español, por defecto) o `"ca"`
#'   (catalán). Si no se especifica, se usa `syll_get_option("lang")`.
#' @return Vector de sílabas en orden, o `NA_character_` si no se pudo
#'   segmentar.
#' @examples
#' syll_split("canción")  # c("can", "ción")
#' syll_split("agradar", lang = "ca")  # c("a", "gra", "dar")
#' @export
#' @family syllabify
syll_split <- function(word, lang = syll_get_option("lang")) {
  w <- clean_word(word, lang = lang)
  if (is.na(w)) return(NA_character_)

  # Caché: clave incluye idioma para no mezclar resultados
  cache_key <- paste0(lang, ":", w)
  if (isTRUE(syll_get_option("cache_enabled"))) {
    cached <- .cache_get(cache_key)
    if (!is.null(cached)) return(cached)
  }

  out <- .syllabify_word(w, lang = lang)

  if (isTRUE(syll_get_option("cache_enabled")) && length(out) && !is.na(out[1L])) {
    .cache_set(cache_key, out)
  }
  out
}

#' Cuenta sílabas de una palabra
#'
#' Envuelve [syll_split()] y devuelve el número de sílabas.
#'
#' @param word Cadena (longitud 1).
#' @param lang Código de idioma: `"es"` o `"ca"`. Por defecto,
#'   `syll_get_option("lang")`.
#' @return Entero con el número de sílabas, o `NA_integer_` si no aplica.
#' @examples
#' syll_count("canción")  # 2
#' syll_count("agradar-me", lang = "ca")  # 4
#' @export
#' @family syllabify
syll_count <- function(word, lang = syll_get_option("lang")) {
  s <- syll_split(word, lang = lang)
  if (length(s) == 1L && is.na(s)) return(NA_integer_)
  length(s)
}

#' Inserta separadores entre sílabas
#'
#' Útil para visualización o verificación de silabificación.
#'
#' @param word Cadena (longitud 1).
#' @param lang Código de idioma: `"es"` o `"ca"`. Por defecto,
#'   `syll_get_option("lang")`.
#' @param hyphen Cadena usada como separador (por defecto `"-"`).
#' @return Cadena con separadores entre sílabas, o `NA` si no se pudo
#'   segmentar.
#' @examples
#' syll_hyphenate("silabificación") # "si-la-bi-fi-ca-ción"
#' syll_hyphenate("agradar-me", lang = "ca")  # "a-gra-dar-me"
#' @export
#' @family syllabify
syll_hyphenate <- function(word, lang = syll_get_option("lang"), hyphen = "-") {
  s <- syll_split(word, lang = lang)
  if (length(s) == 1L && is.na(s)) return(NA_character_)
  paste(s, collapse = hyphen)
}
