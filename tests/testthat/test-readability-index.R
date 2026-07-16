test_that("[readability_index] caso simple: todos los índices por defecto (es)", {
  out <- readability_index("Este es un texto de prueba.")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_setequal(names(out), names(.readability_formula_registry))
  expect_equal(out$szigriszt_pazos, 107.385, tolerance = 1e-6)
})

test_that("[readability_index] pedido de un solo índice", {
  out <- readability_index(c("Texto uno.", "Texto dos un poco m\u00e1s largo."),
                           index = "szigriszt_pazos")
  expect_equal(names(out), "szigriszt_pazos")
  expect_equal(nrow(out), 2L)
})

test_that("[readability_index] pedido de varios índices en una sola llamada", {
  out <- readability_index("Este es un texto de prueba.",
                           index = c("szigriszt_pazos", "crawford"))
  expect_setequal(names(out), c("szigriszt_pazos", "crawford"))
})

test_that("[readability_index] índice inexistente tira error, no NA silencioso", {
  expect_error(readability_index("texto", index = "no_existe"), regexp = "desconocido")
})

test_that("[readability_index] índice válido mas no para el lang pedido: error explícito", {
  expect_error(
    readability_index("Text en catal\u00e0.", lang = "ca", index = "crawford"),
    regexp = "no v\u00e1lido"
  )
})

test_that("[readability_index] lang='ca' sin index explícito: error de 'no hay índices todavía'", {
  expect_error(
    readability_index("Text en catal\u00e0.", lang = "ca"),
    regexp = "No hay \u00edndices.*'ca'"
  )
})

test_that("[readability_index] lang inválido usa el error centralizado de .chk_lang", {
  expect_error(readability_index("texto", lang = "fr"), regexp = "es, ca")
})

test_that("[readability_index] intermediate=TRUE agrega columnas de conteo", {
  out <- readability_index("Este es un texto de prueba.", intermediate = TRUE)
  expect_true(all(c("n_words", "n_sentences", "n_syllables") %in% names(out)))
  expect_equal(out$n_words, 6L)
  expect_equal(out$n_sentences, 1L)
  expect_equal(out$n_syllables, 9L)
})

test_that("[readability_index] intermediate=FALSE (default) no agrega esas columnas", {
  out <- readability_index("Este es un texto de prueba.")
  expect_false(any(c("n_words", "n_sentences", "n_syllables") %in% names(out)))
})

test_that("[readability_index] include_category agrega columna solo donde hay category_fn", {
  out <- readability_index("Este es un texto de prueba.",
                           index = c("szigriszt_pazos", "gutierrez_de_polini"),
                           include_category = TRUE)
  expect_true("szigriszt_pazos_category" %in% names(out))
  expect_false("gutierrez_de_polini_category" %in% names(out))
  expect_equal(out$szigriszt_pazos_category, "Muy f\u00e1cil")
})

test_that("[readability_index] include_category=FALSE (default) no agrega columnas de categoría", {
  out <- readability_index("Este es un texto de prueba.", index = "szigriszt_pazos")
  expect_false("szigriszt_pazos_category" %in% names(out))
})

test_that("[readability_index] doc_id aparece solo si text tiene nombres", {
  sin_nombre <- readability_index(c("Texto uno.", "Texto dos."))
  expect_false("doc_id" %in% names(sin_nombre))

  con_nombre <- readability_index(c(a = "Texto uno.", b = "Texto dos."))
  expect_true("doc_id" %in% names(con_nombre))
  expect_equal(con_nombre$doc_id, c("a", "b"))
})

test_that("[readability_index] NA en el texto se propaga como NA en los scores", {
  expect_warning(
    out <- readability_index(c("Este es un texto de prueba.", NA_character_)),
    regexp = "Mu no puede calcularse"
  )
  expect_false(is.na(out$szigriszt_pazos[1]))
  expect_true(is.na(out$szigriszt_pazos[2]))
})

test_that("[readability_index] mu funciona end-to-end junto al resto de los índices", {
  out <- readability_index("Este es un texto de prueba.", index = "mu")
  expect_equal(out$mu, 135.4839, tolerance = 1e-3)
})

test_that("[readability_index] texto de una sola palabra: mu da NA, el resto no rompe", {
  out <- readability_index("Hola.")
  expect_true(is.na(out$mu))
  expect_false(is.na(out$szigriszt_pazos))
})

test_that("[readability_index] error de text no-character, antes de tocar nada más", {
  expect_error(readability_index(123), regexp = "vector de caracteres")
})

test_that("[readability_index] texto de una sola palabra: mu da NA con warning, el resto no rompe", {
  expect_warning(
    out <- readability_index("Hola.", index = c("szigriszt_pazos", "mu")),
    regexp = "Mu no puede calcularse"
  )
  expect_true(is.na(out$mu))
  expect_false(is.na(out$szigriszt_pazos))
})

test_that("[readability_index] texto vacío da NA en el índice, no un número sin sentido", {
  out <- readability_index("", intermediate = TRUE)
  expect_equal(out$n_words, 0L)
  expect_true(is.na(out$szigriszt_pazos))
  expect_true(is.na(out$gutierrez_de_polini))
})

test_that("[readability_index] intermediate=TRUE incluye letters_per_word", {
  out <- readability_index("Este es un texto de prueba.", intermediate = TRUE)
  expect_true("letters_per_word" %in% names(out))
  expect_equal(out$letters_per_word, 3.5)
})
