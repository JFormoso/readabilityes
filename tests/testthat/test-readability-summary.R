test_that("[readability_summary] valida que data sea data.frame", {
  expect_error(readability_summary(list(id_text = "a", text = "hola")),
               regexp = "data.frame")
})

test_that("[readability_summary] valida que estén las columnas id_text y text", {
  expect_error(
    readability_summary(data.frame(id_text = "a", texto = "hola")),
    regexp = "id_text.*text|text.*id_text"
  )
})

test_that("[readability_summary] caso simple: columnas por defecto y valores correctos", {
  df <- data.frame(id_text = "a", text = "Este es un texto de prueba.")
  out <- readability_summary(df)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)

  expect_setequal(
    names(out),
    c("id_text", "n_words", "n_syllables", "n_sentences",
      "avg_syllables_per_word", "letters_per_word",
      "szigriszt_pazos", "gutierrez_de_polini", "szigriszt_pazos_category")
  )

  expect_equal(out$id_text, "a")
  expect_equal(out$n_words, 6L)
  expect_equal(out$n_syllables, 9L)
  expect_equal(out$n_sentences, 1L)
  expect_equal(out$avg_syllables_per_word, 1.5)
  expect_equal(out$letters_per_word, 3.5)
  expect_equal(out$szigriszt_pazos, 107.385, tolerance = 1e-6)
  expect_equal(out$gutierrez_de_polini, 59.15, tolerance = 1e-6)
  expect_equal(out$szigriszt_pazos_category, "Muy f\u00e1cil")
})

test_that("[readability_summary] no incluye inflesz_score ni inflesz_category (nombres viejos)", {
  df <- data.frame(id_text = "a", text = "Este es un texto de prueba.")
  out <- readability_summary(df)

  expect_false("inflesz_score" %in% names(out))
  expect_false("inflesz_category" %in% names(out))
})

test_that("[readability_summary] texto vacío: conteos en 0, índices en NA", {
  df <- data.frame(id_text = c("a", "b"), text = c("Este es un texto de prueba.", ""))
  out <- readability_summary(df)

  expect_equal(out$n_words[2], 0L)
  expect_true(is.na(out$szigriszt_pazos[2]))
  expect_true(is.na(out$gutierrez_de_polini[2]))
  expect_true(is.na(out$szigriszt_pazos_category[2]))

  # La fila con contenido real no debe verse afectada
  expect_false(is.na(out$szigriszt_pazos[1]))
})

test_that("[readability_summary] texto NA: conteos en NA (no en 0), índices en NA", {
  df <- data.frame(id_text = c("a", "b"),
                   text = c("Este es un texto de prueba.", NA_character_))
  out <- readability_summary(df)

  expect_true(is.na(out$n_words[2]))
  expect_true(is.na(out$szigriszt_pazos[2]))
})

test_that("[readability_summary] permite pedir índices distintos a los del default", {
  df <- data.frame(id_text = "a", text = "Este es un texto de prueba.")
  out <- readability_summary(df, index = c("crawford", "mu"))

  expect_true(all(c("crawford", "mu") %in% names(out)))
  expect_false("szigriszt_pazos" %in% names(out))
  expect_equal(out$crawford, 0.5195, tolerance = 1e-4)
})

test_that("[readability_summary] include_category = FALSE no agrega columna de categoría", {
  df <- data.frame(id_text = "a", text = "Este es un texto de prueba.")
  out <- readability_summary(df, include_category = FALSE)

  expect_false("szigriszt_pazos_category" %in% names(out))
})

test_that("[readability_summary] lang inválido da el error centralizado", {
  df <- data.frame(id_text = "a", text = "hola")
  expect_error(readability_summary(df, lang = "fr"), regexp = "es, ca")
})

test_that("[readability_summary] lang='ca' sin índices válidos da error explícito", {
  df <- data.frame(id_text = "a", text = "Text en catal\u00e0.")
  expect_error(
    readability_summary(df, lang = "ca", index = c("szigriszt_pazos", "gutierrez_de_polini")),
    regexp = "no v\u00e1lido"
  )
})

test_that("[readability_summary] vectoriza sobre múltiples filas preservando el orden de id_text", {
  df <- data.frame(
    id_text = c("x", "y", "z"),
    text    = c("Texto uno.", "Otro texto un poco m\u00e1s largo.", "Tercero.")
  )
  out <- readability_summary(df)

  expect_equal(out$id_text, c("x", "y", "z"))
  expect_equal(nrow(out), 3L)
})
