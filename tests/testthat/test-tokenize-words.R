# tests/testthat/test-tokenize_words.R

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
  out <- tokenize_words(txt)
  expect_type(out, "list")
  expect_length(out, 1)
  toks <- out[[1]]
  expect_true(length(toks) >= 6)
  expect_true(all(toks == tolower(toks)))
  expect_true(all(!grepl("[[:punct:]]", toks)))
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
  keep <- tokenize_words(txt)[[1]]
  expect_true(any(grepl("-", keep)))
  expect_false(any(grepl("[[:punct:]&&[^-]]", keep)))

  split <- tokenize_words(txt, keep_hyphens = FALSE)[[1]]
  expect_false(any(grepl("-", split)))
  expect_true(all(c("auto","piloto","co","autor","siglo","xx","listo") %in% split))

  no_strip <- tokenize_words("hola!", strip_punct = FALSE)[[1]]
  expect_true("hola!" %in% no_strip)
})

test_that("remove_numbers = TRUE descarta tokens puramente numéricos", {
  txt <- "r2d2 año2025 123 45rpm"
  kept <- tokenize_words(txt, remove_numbers = FALSE)[[1]]
  drop <- tokenize_words(txt, remove_numbers = TRUE)[[1]]

  expect_true("123" %in% kept)
  expect_false("123" %in% drop)
  expect_true(all(c("r2d2","año2025","45rpm") %in% kept))
  expect_true(all(c("r2d2","año2025","45rpm") %in% drop))
})

test_that("strip_symbols = TRUE elimina símbolos/emojis", {
  txt <- "café☕ y corazón💖 *precio* 50€"
  kept     <- tokenize_words(txt, strip_symbols = FALSE)[[1]]
  stripped <- tokenize_words(txt, strip_symbols = TRUE)[[1]]

  expect_true(any(grepl("☕|💖|€", paste(kept, collapse = " "))))
  expect_false(any(grepl("☕|💖|€", paste(stripped, collapse = " "))))
  expect_true(all(c("café","y","corazón","precio","50") %in% stripped))
})

test_that("flatten = TRUE retorna vector cuando length(text) == 1", {
  txt <- "Hola mundo cruel"
  out <- tokenize_words(txt, flatten = TRUE)
  expect_type(out, "character")
  expect_gt(length(out), 0)

  out2 <- tokenize_words(c("uno dos", "tres"), flatten = TRUE)
  expect_type(out2, "list")
  expect_length(out2, 2)
})

test_that("combinaciones típicas", {
  txt <- "A la luz del sol"
  toks <- tokenize_words(txt)[[1]]
  expect_true(all(toks == tolower(toks)))
  expect_true(all(c("la","luz","del","sol") %in% toks))
})

test_that("consistencia básica con clean_word/tokenize_clean si existen", {
  skip_if_not(exists("clean_word") && exists("tokenize_clean"))

  words <- c("Hola!", "  NIÑA  ", "maïz", "pingüino", "r2d2")
  for (w in words) {
    toks <- tokenize_words(
      w,
      strip_symbols = TRUE,
      strip_punct   = TRUE,
      keep_hyphens  = TRUE,
      flatten       = TRUE
    )

    cw <- clean_word(w)
    tk_norm <- vapply(toks, clean_word, character(1))
    tk_norm <- tolower(tk_norm[!is.na(tk_norm) & nzchar(tk_norm)])

    if (!is.na(cw) && nzchar(cw)) {
      expect_true(tolower(cw) %in% tk_norm)
    } else {
      expect_length(tk_norm, 0L)
    }

    if (w == "Hola!") {
      tk_nostrip <- tokenize_words(w, strip_punct = FALSE, flatten = TRUE)
      expect_true("hola!" %in% tolower(tk_nostrip))
    }
  }
})


test_that("[CA] lang='ca' activa keep_apostrophes por defecto", {
  # El apóstrofe debe preservarse dentro del token
  toks_ca <- tokenize_words("l'Anna va a l'escola", lang = "ca", flatten = TRUE)
  expect_true(any(grepl("'", toks_ca)))
  expect_true("l'anna" %in% toks_ca)

  # En español el apóstrofe se elimina (comportamiento original)
  toks_es <- tokenize_words("l'Anna", lang = "es", flatten = TRUE)
  expect_false(any(grepl("'", toks_es)))
})

test_that("[CA] Guión se preserva en catalán (keep_hyphens=TRUE por defecto)", {
  toks <- tokenize_words("agradar-me", lang = "ca", flatten = TRUE)
  expect_equal(toks, "agradar-me")
})

test_that("[CA] Apóstrofe tipográfico se normaliza a recto", {
  # U+2019 → '
  toks <- tokenize_words("l\u2019Anna", lang = "ca", flatten = TRUE)
  expect_true("l'anna" %in% toks)
})

test_that("[CA] keep_apostrophes = FALSE en catalán descarta apóstrofes si se pide explícitamente", {
  toks <- tokenize_words("l'Anna", lang = "ca", keep_apostrophes = FALSE, flatten = TRUE)
  expect_false(any(grepl("'", toks)))
})

test_that("[CA] Opción global lang='ca' afecta tokenize_words sin argumento explícito", {
  withr::local_options(list("syll.lang" = "ca"))
  toks <- tokenize_words("l'Anna va a l'escola", flatten = TRUE)
  expect_true(any(grepl("'", toks)))
})

test_that("[CA] Texto catalán con letras específicas (à, è, ò) se tokeniza correctamente", {
  toks <- tokenize_words("Pa\u00efsos Catalans i m\u00e0 d'obra", lang = "ca", flatten = TRUE)
  expect_true("pa\u00efsos" %in% toks) # països
  expect_true("m\u00e0"     %in% toks) # mà
})
