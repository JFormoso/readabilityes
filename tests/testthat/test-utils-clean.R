# tests/testthat/test-utils_clean.R

test_that("clean_word: maneja entradas inválidas y bordes", {
  expect_identical(clean_word(NA), NA_character_)
  expect_identical(clean_word(character()), NA_character_)
  expect_identical(clean_word(c("a","b")), NA_character_)
  expect_identical(clean_word(""), NA_character_)
  expect_identical(clean_word("   "), NA_character_)
})

test_that("clean_word: normaliza minúsculas y recorta espacios", {
  expect_identical(clean_word("  Hola  "), "hola")
  expect_identical(clean_word("\tÁRBOL\n"), "árbol")
})

test_that("clean_word: conserva letras válidas (tildes, ñ, diéresis) y elimina resto", {
  expect_identical(clean_word("canci\u00f3n!"), "canción")
  expect_identical(clean_word("ping\u00fcino"), "pingüino")
  expect_identical(clean_word("ma\u00efz"), "maïz")
  expect_identical(clean_word("Ni\u00f1a?"), "niña")
  expect_identical(clean_word("a,b.c;d:e"), "abcde")
})

test_that("clean_word: elimina números y símbolos dejando solo letras válidas", {
  expect_identical(clean_word("r2d2"), "rd")
  expect_identical(clean_word("hola123"), "hola")
  expect_identical(clean_word("f\u00fatbol-2025"), "fútbol")
  expect_identical(clean_word("co$st@e!"), "coste")
})

# NUEVO -----------------------------------------------------------------------
test_that("[CA] clean_word: conserva letras catalanas específicas (à, è, ò)", {
  expect_identical(clean_word("m\u00e0", lang = "ca"),   "m\u00e0")   # mà
  expect_identical(clean_word("s\u00e8t", lang = "ca"),  "s\u00e8t")  # sèt
  expect_identical(clean_word("s\u00f2l", lang = "ca"),  "s\u00f2l")  # sòl
  expect_identical(clean_word("pa\u00efsos", lang = "ca"), "pa\u00efsos") # països
})

test_that("[CA] clean_word: preserva apóstrofe en catalán y lo normaliza", {
  # Apóstrofe recto
  expect_identical(clean_word("l'anna", lang = "ca"), "l'anna")
  # Apóstrofe tipográfico → recto
  expect_identical(clean_word("l\u2019anna", lang = "ca"), "l'anna")
  # Guión también se preserva
  expect_identical(clean_word("agradar-me", lang = "ca"), "agradar-me")
})

test_that("[CA] clean_word: lang='es' NO permite letras exclusivas del catalán", {
  # à, è, ò no existen en español → deben eliminarse
  expect_identical(clean_word("m\u00e0", lang = "es"), "m")
  expect_identical(clean_word("s\u00e8t", lang = "es"), "st")
})

test_that("[CA] clean_word: lang='es' sigue comportándose igual que antes", {
  # Regresión: los tests originales deben seguir pasando con lang="es" explícito
  expect_identical(clean_word("canci\u00f3n!", lang = "es"), "canción")
  expect_identical(clean_word("ping\u00fcino", lang = "es"), "pingüino")
})
# FIN NUEVO -------------------------------------------------------------------

test_that("is_valid_word: TRUE solo si la entrada ya es válida", {
  expect_identical(is_valid_word("hola"), TRUE)
  expect_identical(is_valid_word("  HOLA  "), TRUE)
  expect_identical(is_valid_word("hola!"), FALSE)
  expect_identical(is_valid_word("niña"), TRUE)
  expect_identical(is_valid_word("pingüino"), TRUE)
  expect_identical(is_valid_word("maïz"), TRUE)
  expect_true(is.na(is_valid_word(NA)))
  expect_true(is.na(is_valid_word(character())))
  expect_identical(is_valid_word("r2d2"), FALSE)
})

# NUEVO -----------------------------------------------------------------------
test_that("[CA] is_valid_word acepta palabras catalanas con lang='ca'", {
  expect_identical(is_valid_word("pa\u00efsos", lang = "ca"), TRUE)  # països
  expect_identical(is_valid_word("m\u00e0",     lang = "ca"), TRUE)  # mà
  # El apóstrofe hace que no sea "válida" en sentido estricto (no es letra)
  expect_identical(is_valid_word("l'anna",      lang = "ca"), FALSE)
})
# FIN NUEVO -------------------------------------------------------------------

test_that("tokenize_clean: tokeniza por separadores y limpia correctamente", {
  txt <- "¡Hola, mundo—cruel! Año 2025: pingüino, maïz y NIÑA."
  toks <- tokenize_clean(txt)
  expect_type(toks, "character")
  expect_setequal(toks, c("hola","mundo","cruel","año","pingüino","maïz","y","niña"))
  expect_false(any(is.na(toks)))
  expect_true(all(nzchar(toks)))
})

test_that("tokenize_clean: maneja guiones, apóstrofes, emojis y separadores raros", {
  txt <- "auto-piloto «entre» comillas… café☕ y corazón💖 O'Neill"
  toks <- tokenize_clean(txt)
  expect_setequal(toks, c("auto","piloto","entre","comillas","café","y","corazón","o","neill"))
})

test_that("tokenize_clean: entradas inválidas devuelven vector vacío", {
  expect_identical(tokenize_clean(NA), character())
  expect_identical(tokenize_clean(character()), character())
  expect_identical(tokenize_clean(c("a","b")), character())
})

test_that("coherencia clean_word / is_valid_word / tokenize_clean (por palabra)", {
  words <- c("Hola!", "  NIÑA  ", "maïz", "pingüino", "r2d2", NA_character_, "")
  for (w in words) {
    toks <- tokenize_clean(w)
    cw   <- clean_word(w)

    if (is.na(cw) || !nzchar(cw)) {
      expect_length(toks, 0)
    } else {
      expect_identical(paste0(toks, collapse = ""), cw)
      expect_true(all(vapply(toks, is_valid_word, logical(1), USE.NAMES = FALSE)))
    }
  }
})
