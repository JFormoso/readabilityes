test_that("tokenize_words existe y es función", {
  expect_true(exists("tokenize_words"))
  expect_true(is.function(tokenize_words))
})

test_that("entrada inválida lanza error; vector vacío devuelve lista vacía", {
  expect_error(tokenize_words(NA), regexp = "debe ser un vector de caracteres")
  out <- tokenize_words(character())
  expect_type(out, "list")
  expect_length(out, 0)
})

test_that("por defecto: minúsculas, tildes preservadas, puntuación fuera, guiones dentro", {
  txt <- "¡Hola, mundo! Año 2025: pingüino, maïz y NIÑA."
  out <- tokenize_words(txt)            # lista de longitud 1
  expect_type(out, "list")
  expect_length(out, 1)
  toks <- out[[1]]
  expect_true(length(toks) >= 6)
  # lowercase = TRUE
  expect_true(all(toks == tolower(toks)))
  # strip_punct = TRUE, keep_hyphens = TRUE
  expect_true(all(!grepl("[[:punct:]]", toks)))
  # keep_accents = TRUE
  expect_true(all(c("hola","mundo","año","pingüino","maïz","niña") %in% toks))
})

test_that("lowercase = FALSE conserva mayúsculas", {
  txt <- "Hola NIÑA Café"
  toks <- tokenize_words(txt, lowercase = FALSE)[[1]]
  expect_true(any(grepl("[A-ZÁÉÍÓÚÑ]", toks)))
})

test_that("keep_accents = FALSE elimina tildes/diéresis", {
  txt <- "canción pingüino maïz NIÑA"
  kept     <- tokenize_words(txt, keep_accents = TRUE)[[1]]
  stripped <- tokenize_words(txt, keep_accents = FALSE)[[1]]
  expect_true(all(c("canción","pingüino","maïz","niña") %in% kept))
  expect_true(all(c("cancion","pinguino","maiz","nina") %in% stripped))
})

test_that("strip_punct y keep_hyphens: control de puntuación y guiones", {
  txt <- "auto-piloto, co-autor; siglo-XX. ¡Listo?"
  # default (strip_punct=TRUE, keep_hyphens=TRUE) conserva '-'
  keep <- tokenize_words(txt)[[1]]
  expect_true(any(grepl("-", keep)))
  expect_false(any(grepl("[[:punct:]&&[^-]]", keep)))

  # sin conservar guiones: se separa en tokens sin '-'
  split <- tokenize_words(txt, keep_hyphens = FALSE)[[1]]
  expect_false(any(grepl("-", split)))
  expect_true(all(c("auto","piloto","co","autor","siglo","xx","listo") %in% split))

  # sin quitar puntuación: quedan signos pegados
  no_strip <- tokenize_words("hola!", strip_punct = FALSE)[[1]]
  expect_true("hola!" %in% no_strip)
})

test_that("remove_numbers = TRUE descarta tokens puramente numéricos", {
  txt <- "r2d2 año2025 123 45rpm"
  kept <- tokenize_words(txt, remove_numbers = FALSE)[[1]]
  drop <- tokenize_words(txt, remove_numbers = TRUE)[[1]]

  expect_true("123" %in% kept)
  expect_false("123" %in% drop)

  # tokens alfanuméricos se conservan en ambos casos
  expect_true(all(c("r2d2","año2025","45rpm") %in% kept))
  expect_true(all(c("r2d2","año2025","45rpm") %in% drop))
})

test_that("strip_symbols = TRUE elimina símbolos/emojis", {
  txt <- "café☕ y corazón💖 *precio* 50€"
  kept   <- tokenize_words(txt, strip_symbols = FALSE)[[1]]
  stripped <- tokenize_words(txt, strip_symbols = TRUE)[[1]]

  # Sin quitar símbolos, pueden quedar pegados
  expect_true(any(grepl("☕|💖|€", paste(kept, collapse = " "))))
  # Quitando símbolos, no deberían aparecer
  expect_false(any(grepl("☕|💖|€", paste(stripped, collapse = " "))))

  # El contenido léxico debe seguir presente
  expect_true(all(c("café","y","corazón","precio","50") %in% stripped))
})

test_that("flatten = TRUE retorna vector cuando length(text) == 1", {
  txt <- "Hola mundo cruel"
  out <- tokenize_words(txt, flatten = TRUE)
  expect_type(out, "character")
  expect_gt(length(out), 0)

  # Si hay más de un elemento en text, flatten sigue devolviendo lista (según tu implementación)
  out2 <- tokenize_words(c("uno dos", "tres"), flatten = TRUE)
  expect_type(out2, "list")
  expect_length(out2, 2)
})

test_that("combinaciones típicas: min_len conceptual (no existe), pero se puede emular", {
  # No tenés min_len; emulemos filtrando nchar >= 3 post-tokenización
  txt <- "A la luz del sol"
  toks <- tokenize_words(txt)[[1]]
  expect_true(all(toks == tolower(toks)))
  expect_true(all(c("la","luz","del","sol") %in% toks))
  toks3 <- toks[nchar(toks) >= 3]
  expect_true(all(nchar(toks3) >= 3))
})

test_that("consistencia básica con clean_word/tokenize_clean si existen", {
  skip_if_not(exists("clean_word") && exists("tokenize_clean"))

  words <- c("Hola!", "  NIÑA  ", "maïz", "pingüino", "r2d2")
  for (w in words) {
    # Alineamos políticas razonables con clean_word:
    toks <- tokenize_words(
      w,
      strip_symbols = TRUE,  # suele limpiar símbolos como hace clean_word
      strip_punct   = TRUE,
      keep_hyphens  = TRUE,
      flatten       = TRUE
    )

    cw <- clean_word(w)
    # Normalizamos cada token con clean_word y filtramos vacíos/NA
    tk_norm <- vapply(toks, clean_word, character(1))
    tk_norm <- tolower(tk_norm[!is.na(tk_norm) & nzchar(tk_norm)])

    if (!is.na(cw) && nzchar(cw)) {
      # La versión limpia de la palabra debe aparecer entre los tokens limpiados
      expect_true(tolower(cw) %in% tk_norm)
    } else {
      # Si clean_word deja vacío/NA, entonces ningún token limpio debería quedar
      expect_length(tk_norm, 0L)
    }

    # Verificación adicional: si NO removemos puntuación, el signo se conserva
    if (w == "Hola!") {
      tk_nostrip <- tokenize_words(w, strip_punct = FALSE, flatten = TRUE)
      expect_true("hola!" %in% tolower(tk_nostrip))
    }
  }
})


