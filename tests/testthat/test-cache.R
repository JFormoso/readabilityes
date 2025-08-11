# tests/testthat/test-cache.R

test_that("syll_cache_clear() vacía el caché y es invisible", {
  # Deja el caché limpio al inicio y al final del test
  syll_cache_clear()
  withr::defer(syll_cache_clear())

  # Debe ser invisible y devolver NULL
  expect_invisible(syll_cache_clear())

  info <- syll_cache_info()
  expect_type(info, "list")
  expect_named(info, c("size", "keys"))
  expect_equal(info$size, 0L)
  expect_type(info$keys, "character")
  expect_length(info$keys, 0L)
})

test_that(".cache_get() devuelve NULL si la clave no existe", {
  syll_cache_clear()
  withr::defer(syll_cache_clear())

  expect_null(readabilityes:::.cache_get("no-existe"))
})

test_that(".cache_set() y .cache_get() funcionan en roundtrip", {
  syll_cache_clear()
  withr::defer(syll_cache_clear())

  key <- paste0("k_", as.integer(runif(1, 1, 1e6)))
  val1 <- list(syl = c("ba", "la"))

  readabilityes:::.cache_set(key, val1)
  expect_equal(readabilityes:::.cache_get(key), val1)

  info <- syll_cache_info()
  expect_equal(info$size, 1L)
  expect_true(key %in% info$keys)

  # Sobrescritura mantiene tamaño y actualiza el valor
  val2 <- "nuevo"
  readabilityes:::.cache_set(key, val2)
  expect_equal(readabilityes:::.cache_get(key), val2)
  expect_equal(syll_cache_info()$size, 1L)
})

test_that(".cache_clear() elimina todas las claves", {
  syll_cache_clear()
  withr::defer(syll_cache_clear())

  readabilityes:::.cache_set("a", 1)
  readabilityes:::.cache_set("b", 2)

  info_before <- syll_cache_info()
  expect_equal(info_before$size, 2L)
  expect_true(all(c("a", "b") %in% info_before$keys))

  # Limpiar vía API pública
  expect_invisible(syll_cache_clear())

  info_after <- syll_cache_info()
  expect_equal(info_after$size, 0L)
  expect_length(info_after$keys, 0L)

  expect_null(readabilityes:::.cache_get("a"))
  expect_null(readabilityes:::.cache_get("b"))
})
