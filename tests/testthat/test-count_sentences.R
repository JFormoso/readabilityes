# tests/testthat/test-count_sentences.R

test_that("valida tipo de entrada", {
  expect_error(count_sentences(123), "`text` debe ser un vector de caracteres")
})

test_that("cuenta correctamente oraciones básicas", {
  txt <- "Hola. ¿Cómo estás? ¡Todo bien!"
  expect_equal(count_sentences(txt), 3L)

  txt_vec <- c("Una sola.", "Dos. Sí.")
  expect_equal(count_sentences(txt_vec), c(1L, 2L))
})

test_that("maneja NA según na_as_zero", {
  txt <- c("Una sola.", NA)
  # Por defecto NA se mantiene
  expect_equal(count_sentences(txt), c(1L, NA_integer_))
  # Con na_as_zero = TRUE, NA se convierte a 0
  expect_equal(count_sentences(txt, na_as_zero = TRUE), c(1L, 0L))
})

test_that("drop_empty = TRUE descarta oraciones vacías", {
  txt <- "Hola.   . Chau."
  # Con drop_empty = TRUE (por defecto), se cuentan solo no vacías
  expect_equal(count_sentences(txt, drop_empty = TRUE), 2L)
})

test_that("drop_empty = FALSE conserva segmentos vacíos", {
  txt <- "Hola.   . Chau."
  # Hay 3 segmentos: "Hola.", ".", "Chau."
  expect_equal(count_sentences(txt, drop_empty = FALSE), 3L)
})

test_that("vectoriza sobre múltiples entradas", {
  txt <- c("Uno.", "Dos. Tres.", "Cuatro. Cinco. Seis.")
  expect_equal(count_sentences(txt), c(1L, 2L, 3L))
})

test_that("oraciones con abreviaturas no se cortan indebidamente", {
  txt <- "El Sr. Gómez llegó. La Dra. Pérez también."
  expect_equal(count_sentences(txt), 2L)
})

test_that("maneja entradas vacías devolviendo 0 o NA según na_as_zero", {
  txt <- ""
  expect_equal(count_sentences(txt), 0L) # no NA porque no es NA_character_
  expect_equal(count_sentences(NA_character_, na_as_zero = TRUE), 0L)
})
