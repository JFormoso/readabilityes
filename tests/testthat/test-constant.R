test_that(".valid_letters contiene solo minúsculas válidas y sin duplicados", {
  expect_type(.valid_letters, "character")
  expect_length(.valid_letters, 1)
  letters_vec <- strsplit(.valid_letters, "", fixed = TRUE)[[1]]

  # Deben ser minúsculas
  expect_true(all(letters_vec == tolower(letters_vec)))

  # Incluir caracteres esperables del español
  expect_true(all(c("ñ", "á", "é", "í", "ó", "ú", "ü") %in% letters_vec))

  # No debe haber duplicados
  expect_equal(length(unique(letters_vec)), length(letters_vec))

  # Codificación UTF-8 (o forzar y comparar)
  expect_identical(paste(letters_vec, collapse = ""), enc2utf8(.valid_letters))
})

test_that("vocales fuertes/débiles/tilde son coherentes", {
  valid_vec <- strsplit(.valid_letters, "", fixed = TRUE)[[1]]

  for (x in list(.v_fuerte, .v_debil, .v_tilde)) {
    expect_type(x, "character")
    expect_gt(length(x), 0)
    expect_true(all(x == tolower(x)))
    expect_true(all(x %in% valid_vec))
    expect_equal(length(unique(x)), length(x))
  }

  # Las tildes deben ser tildes de las cinco vocales
  expect_true(all(c("á","é","í","ó","ú") %in% .v_tilde))

  # Las fuertes incluyen a/e/o (con y sin tilde)
  expect_true(all(c("a","á","e","é","o","ó") %in% .v_fuerte))

  # Las débiles incluyen i/u y sus variantes no tónicas con diéresis
  expect_true(all(c("i","u","ï","ü") %in% .v_debil))

  # Opcional: confirmar que í/ú estén en .v_tilde pero no en débiles
  expect_true(all(c("í","ú") %in% .v_tilde))
  expect_false(any(c("í","ú") %in% .v_debil))
})

test_that(".clusters_lr tiene pares consonánticos con l/r y válidos", {
  expect_type(.clusters_lr, "character")
  expect_gt(length(.clusters_lr), 0)
  expect_equal(length(unique(.clusters_lr)), length(.clusters_lr)) # sin duplicados
  # Todos de longitud 2, terminan en l/r, y usan solo letras válidas
  valid <- strsplit(.valid_letters, "", fixed = TRUE)[[1]]
  all_ok <- vapply(.clusters_lr, function(cl) {
    nchar(cl) == 2 &&
      grepl("[lr]$", cl) &&
      all(strsplit(cl, "", fixed = TRUE)[[1]] %in% valid)
  }, logical(1))
  expect_true(all(all_ok))
  # No deberían contener vocales
  any_vowel <- vapply(.clusters_lr, function(cl) grepl("[aeiouáéíóúü]", cl), logical(1))
  expect_false(any(any_vowel))
})

test_that("regex precompiladas están inicializadas luego de .onLoad()", {
  # Si alguna está en NULL, se salta (útil en load_all() sin onLoad)
  skip_if(is.null(.rx_valid_letters) || is.null(.rx_vowel) ||
            is.null(.rx_strong) || is.null(.rx_weak),
          message = "Regex no inicializadas (¿.onLoad() corrió?)")

  # Asumimos que se guardan como patrones de texto no vacíos
  expect_type(.rx_valid_letters, "character"); expect_length(.rx_valid_letters, 1); expect_true(nzchar(.rx_valid_letters))
  expect_type(.rx_vowel, "character");          expect_length(.rx_vowel, 1);          expect_true(nzchar(.rx_vowel))
  expect_type(.rx_strong, "character");         expect_length(.rx_strong, 1);         expect_true(nzchar(.rx_strong))
  expect_type(.rx_weak, "character");           expect_length(.rx_weak, 1);           expect_true(nzchar(.rx_weak))

  # Smoke tests básicos: que "encajen" con algo esperable
  expect_true(grepl(.rx_vowel, "a"))
  expect_true(grepl(.rx_strong, "á"))  # fuerte con tilde
  expect_true(grepl(.rx_weak,   "ü"))  # débil con diéresis
})

test_that("caché de sílabas es un entorno (si fue creado)", {
  skip_if(is.null(.syll_cache), message = "Caché no inicializado (¿.onLoad() corrió?)")
  expect_true(is.environment(.syll_cache))
})
