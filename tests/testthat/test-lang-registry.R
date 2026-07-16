test_that("[registro] .lang_registry contiene entradas para 'es' y 'ca'", {
  reg <- readabilityes:::.lang_registry
  expect_true(is.list(reg))
  expect_setequal(names(reg), c("es", "ca"))
})

test_that("[registro] .lang_get('es') devuelve una entrada completa y coherente", {
  entry <- readabilityes:::.lang_get("es")

  expect_type(entry, "list")
  expect_named(
    entry,
    c("label", "valid_letters", "v_fuerte", "v_debil", "v_tilde", "clusters", "syllabify_fn", "sentence_abbr"),
    ignore.order = TRUE
  )

  expect_identical(entry$valid_letters, readabilityes:::.valid_letters)
  expect_identical(entry$v_fuerte,      readabilityes:::.v_fuerte)
  expect_identical(entry$v_debil,       readabilityes:::.v_debil)
  expect_identical(entry$v_tilde,       readabilityes:::.v_tilde)
  expect_identical(entry$clusters,      readabilityes:::.clusters_lr)
  expect_identical(entry$sentence_abbr, readabilityes:::.abbr_es)
  expect_true(is.function(entry$syllabify_fn))
})

test_that("[registro] .lang_get('ca') devuelve una entrada completa y coherente", {
  entry <- readabilityes:::.lang_get("ca")

  expect_type(entry, "list")
  expect_named(
    entry,
    c("label", "valid_letters", "v_fuerte", "v_debil", "v_tilde", "clusters", "syllabify_fn", "sentence_abbr"),
    ignore.order = TRUE
  )

  expect_identical(entry$valid_letters, readabilityes:::.valid_letters_ca)
  expect_identical(entry$v_fuerte,      readabilityes:::.v_fuerte_ca)
  expect_identical(entry$v_debil,       readabilityes:::.v_debil_ca)
  expect_identical(entry$v_tilde,       readabilityes:::.v_tilde_ca)
  expect_identical(entry$clusters,      readabilityes:::.clusters_ca)
  expect_identical(entry$sentence_abbr, readabilityes:::.abbr_ca)
  expect_true(is.function(entry$syllabify_fn))
})

test_that("[registro] syllabify_fn de cada idioma silabifica correctamente", {
  # Sanity check: el registro apunta a la función correcta, no a una copia
  # o a la del otro idioma por error de copy-paste.
  es_fn <- readabilityes:::.lang_get("es")$syllabify_fn
  ca_fn <- readabilityes:::.lang_get("ca")$syllabify_fn

  expect_equal(es_fn("cancion"), c("can", "cion"))
  expect_equal(ca_fn("amic"),    c("a", "mic"))
})

test_that("[registro] .lang_get() con idioma no soportado tira error centralizado", {
  expect_error(
    readabilityes:::.lang_get("fr"),
    regexp = "Idioma no soportado: 'fr'"
  )
  expect_error(
    readabilityes:::.lang_get("fr"),
    regexp = "es, ca"
  )
})

test_that("[registro] .lang_get() con idioma vacío o inválido no rompe de forma rara", {
  expect_error(readabilityes:::.lang_get(""))
  expect_error(readabilityes:::.lang_get(NA_character_))
})
