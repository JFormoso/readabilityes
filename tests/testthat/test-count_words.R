# tests/testthat/test-count_words.R

test_that("valida tipo de entrada", {
  expect_error(count_words(123), "`text` debe ser un vector de caracteres")
})

test_that("cuenta correctamente palabras en casos simples", {
  expect_equal(count_words("Este es un ejemplo sencillo."), 5L)
  expect_equal(count_words(c("Primera frase.", "Segunda frase con más palabras.")),
               c(2L, 5L))
})

test_that("maneja puntuación y múltiples espacios", {
  expect_equal(count_words("Hola,   mundo."), 2L)
  expect_equal(count_words("  Mucho   espacio   interno  "), 3L)
})

test_that("tildes y eñes no rompen la tokenización", {
  expect_equal(count_words("Información útil para programación en español."), 6L)
})

test_that("cadena vacía devuelve 0 (no NA)", {
  expect_equal(count_words(""), 0L)
})

test_that("manejo de NA según na_as_zero", {
  x <- c("Una frase.", NA_character_, "Otra más.")
  expect_equal(count_words(x), c(2L, NA_integer_, 2L))
  expect_equal(count_words(x, na_as_zero = TRUE), c(2L, 0L, 2L))
})

test_that("vectoriza sobre múltiples entradas", {
  v <- c("Una.", "Dos palabras.", "Tres palabras aquí.")
  expect_equal(count_words(v), c(1L, 2L, 3L))
})
