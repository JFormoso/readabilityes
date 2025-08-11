# tests/testthat/test-count_syllables.R

# Helpers para tests (silabificadores dummy)
syll_one <- function(w) {
  # Devuelve 1 si el token no está vacío (útil para probar tokenización)
  if (is.na(w) || !nzchar(w)) return(0L)
  1L
}

syll_len <- function(w, weight = 1) {
  # "Cuenta sílabas" como cantidad de letras A-Z (incluye tildes y ñ) * weight
  # Sólo para verificar que syll_args se pasa correctamente
  letters_only <- gsub("[^[:alpha:]]+", "", w, perl = TRUE)
  nchar(letters_only) * weight
}

test_that("valida tipos de argumentos", {
  expect_error(count_syllables(123), "`text` debe ser un vector de caracteres")
  expect_error(count_syllables("ok", syllable_fun = 1), "`syllable_fun` debe ser una función")
  expect_error(count_syllables("ok", syll_args = 1), "`syll_args` debe ser una lista")
})

test_that("cuenta tokens básicos con un silabificador trivial", {
  # Con syll_one cada palabra vale 1
  expect_equal(count_syllables("uno dos tres", syllable_fun = syll_one), 3L)
  expect_equal(count_syllables(c("hola mundo", "qué tal"), syll_one),
               c(2L, 2L))
})

test_that("maneja puntuación y espacios con drop_empty", {
  # Tokenización típica: "Hola, mundo." -> c("Hola", "mundo")
  expect_equal(count_syllables("Hola,   mundo.", syll_one), 2L)

  # Si la entrada está vacía, 0 (no NA porque no es NA_character_)
  expect_equal(count_syllables("", syll_one), 0L)

  # Segments raros (si el tokenizador devolviera tokens vacíos) se descartan con drop_empty = TRUE
  # (no podemos forzar tokens vacíos sin tocar tokenize_words, pero dejamos el caso cubierto)
  expect_equal(count_syllables("   ", syll_one), 0L)
})

test_that("manejo de NA según na_as_zero", {
  x <- c("uno dos", NA_character_, "tres")
  # Por defecto NA se conserva
  expect_equal(count_syllables(x, syll_one), c(2L, NA_integer_, 1L))
  # Con na_as_zero = TRUE, NA -> 0
  expect_equal(count_syllables(x, syll_one, na_as_zero = TRUE), c(2L, 0L, 1L))
})

test_that("syll_args se propaga a syllable_fun", {
  # Con syll_len y weight=2, "ab cd" tiene 2+2 letras => (4)*2 = 8
  expect_equal(count_syllables("ab cd", syll_len, syll_args = list(weight = 2)), 8L)
  # Sin weight (1 por defecto): 4
  expect_equal(count_syllables("ab cd", syll_len), 4L)
})

test_that("vectoriza sobre múltiples entradas", {
  v <- c("uno dos", "tres", "cuatro cinco seis")
  expect_equal(count_syllables(v, syll_one), c(2L, 1L, 3L))
})

test_that("integración mínima con syll_count (por palabra) y suma esperada", {
  # La suma por texto debe ser la suma de syll_count por palabra
  txt <- "canción prueba"
  toks <- tokenize_words(txt)[[1]]
  esperado <- sum(vapply(toks, syll_count, integer(1)))
  expect_equal(count_syllables(txt), esperado)
})

test_that("un solo token se comporta igual que syll_count", {
  # Si hay un único token, el total debe igualar syll_count(token)
  expect_equal(count_syllables("pingüino"), syll_count("pingüino"))
  expect_equal(count_syllables("extraordinario"), syll_count("extraordinario"))
})
