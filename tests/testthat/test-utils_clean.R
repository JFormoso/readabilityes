test_that("clean_word: maneja entradas inválidas y bordes", {
  expect_identical(clean_word(NA), NA_character_)
  expect_identical(clean_word(character()), NA_character_)
  expect_identical(clean_word(c("a","b")), NA_character_)  # length > 1
  expect_identical(clean_word(""), NA_character_)          # queda vacío tras limpiar
  expect_identical(clean_word("   "), NA_character_)       # solo espacios
})

test_that("clean_word: normaliza minúsculas y recorta espacios", {
  expect_identical(clean_word("  Hola  "), "hola")
  expect_identical(clean_word("\tÁRBOL\n"), "árbol")
})

test_that("clean_word: conserva letras válidas (tildes, ñ, diéresis) y elimina resto", {
  expect_identical(clean_word("canci\u00f3n!"), "canción")  # "canción!"
  expect_identical(clean_word("ping\u00fcino"), "pingüino") # "pingüino"
  expect_identical(clean_word("ma\u00efz"), "maïz")         # "maïz" (ï)
  expect_identical(clean_word("Ni\u00f1a?"), "niña")        # "Niña?"
  expect_identical(clean_word("a,b.c;d:e"), "abcde")        # signos fuera
})

test_that("clean_word: elimina números y símbolos dejando solo letras válidas", {
  expect_identical(clean_word("r2d2"), "rd")
  expect_identical(clean_word("hola123"), "hola")
  expect_identical(clean_word("f\u00fatbol-2025"), "fútbol") # "fútbol-2025"
  expect_identical(clean_word("co$st@e!"), "coste")
})

test_that("is_valid_word: TRUE solo si la entrada ya es válida (tras trim + tolower) y sin limpieza adicional", {
  expect_identical(is_valid_word("hola"), TRUE)
  expect_identical(is_valid_word("  HOLA  "), TRUE)  # solo cambia por tolower/trim, sigue válida
  expect_identical(is_valid_word("hola!"), FALSE)    # se eliminaría '!' -> inválida
  expect_identical(is_valid_word("niña"), TRUE)
  expect_identical(is_valid_word("pingüino"), TRUE)
  expect_identical(is_valid_word("maïz"), TRUE)
  expect_true(is.na(is_valid_word(NA)))
  expect_true(is.na(is_valid_word(character())))
  expect_identical(is_valid_word("r2d2"), FALSE)    # dígitos no son válidos
})

test_that("tokenize_clean: tokeniza por separadores y limpia correctamente", {
  txt <- "¡Hola, mundo—cruel! Año 2025: pingüino, maïz y NIÑA."
  toks <- tokenize_clean(txt)
  expect_type(toks, "character")
  # Debe contener solo palabras limpias, en minúscula, sin signos/nums
  expect_setequal(toks, c("hola","mundo","cruel","año","pingüino","maïz","y","niña"))

  # Sin NAs ni cadenas vacías
  expect_false(any(is.na(toks)))
  expect_true(all(nzchar(toks)))
})

test_that("tokenize_clean: maneja guiones, apóstrofes, emojis y separadores raros", {
  txt <- "auto-piloto «entre» comillas… café☕ y corazón💖 O'Neill"
  toks <- tokenize_clean(txt)
  # `O'Neill` se divide y se limpia (apóstrofe se elimina)
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
      # Si clean_word da NA o vacío, tokenizar esa misma palabra debe dar 0 tokens
      expect_length(toks, 0)
    } else {
      # Si hay contenido, concatenar tokens debe reconstruir clean_word(word)
      expect_identical(paste0(toks, collapse = ""), cw)
      # Además, todos los tokens deben ser válidos
      expect_true(all(vapply(toks, is_valid_word, logical(1), USE.NAMES = FALSE)))
    }
  }
})
