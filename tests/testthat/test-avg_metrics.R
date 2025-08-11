# tests/testthat/test-avg_metrics.R

# ---- Helpers para tests ----
syll_one <- function(w) {
  if (is.na(w) || !nzchar(w)) return(0L)
  1L
}

test_that("valida tipo de entrada", {
  expect_error(avg_syllables_per_word(123), "`text` debe ser un vector de caracteres")
  expect_error(letters_per_word(123), "`text` debe ser un vector de caracteres")
  expect_error(chars_per_word(123), "`text` debe ser un vector de caracteres")
})

# ---- avg_syllables_per_word ----

test_that("avg_syllables_per_word: promedio simple con silabificador trivial", {
  # Cada palabra vale 1 sílaba => el promedio debe ser 1
  expect_equal(avg_syllables_per_word("uno dos", syllable_fun = syll_one), 1)
  expect_equal(
    avg_syllables_per_word(c("hola mundo", "tres palabras aquí"), syllable_fun = syll_one),
    c(1, 1)
  )
})

test_that("avg_syllables_per_word: NA y entradas vacías según na_as_zero", {
  x <- c("hola mundo", NA_character_, "")
  # Por defecto: NA se mantiene; "" no tiene palabras => NA
  expect_equal(
    avg_syllables_per_word(x, syllable_fun = syll_one),
    c(1, NA_real_, NA_real_)
  )
  # Con na_as_zero = TRUE: NA y "" -> 0
  expect_equal(
    avg_syllables_per_word(x, syllable_fun = syll_one, na_as_zero = TRUE),
    c(1, 0, 0)
  )
})

test_that("avg_syllables_per_word: coherencia con count_syllables / count_words", {
  v <- c("canción de prueba", "texto", "", NA_character_)
  total_syl   <- count_syllables(v)
  total_words <- count_words(v)

  # Donde hay palabras (>0) y no hay NA en los conteos
  ok <- total_words > 0 & !is.na(total_syl) & !is.na(total_words)
  esperado <- total_syl[ok] / total_words[ok]
  obtenido <- avg_syllables_per_word(v)[ok]

  expect_equal(obtenido, esperado, tolerance = 1e-12)
})

# ---- letters_per_word ----

test_that("letters_per_word: casos simples y con tildes/eñes", {
  # "Hola mundo" -> 4 y 5 letras => 4.5
  expect_equal(letters_per_word("Hola mundo!"), 4.5, tolerance = 1e-12)

  # Diacríticos cuentan como letras
  # "Información" (11) y "útil" (4) => (11+4)/2 = 7.5
  expect_equal(letters_per_word("Información útil"), 7.5, tolerance = 1e-12)
})

test_that("letters_per_word: NA y cadenas sin palabras", {
  x <- c("  ", "", NA_character_, "uno")
  # Por defecto: "" y "  " no tienen palabras => NA; NA se mantiene
  expect_equal(letters_per_word(x), c(NA_real_, NA_real_, NA_real_, 3))
  # Con na_as_zero: NAs (incluye sin palabras) se vuelven 0
  expect_equal(letters_per_word(x, na_as_zero = TRUE), c(0, 0, 0, 3))
})

test_that("letters_per_word: vectoriza", {
  v <- c("aa bb", "ccc", "d e f")
  # (2+2)/2 = 2 ; 3/1 = 3 ; (1+1+1)/3 = 1
  expect_equal(letters_per_word(v), c(2, 3, 1))
})

# ---- chars_per_word ----

test_that("chars_per_word: casos simples", {
  # "Hola mundo" -> 4 y 5 caracteres => 4.5
  expect_equal(chars_per_word("Hola mundo"), 4.5, tolerance = 1e-12)

  # Con palabras cortas
  expect_equal(chars_per_word(c("a bc", "def")), c(1.5, 3), tolerance = 1e-12)
})

test_that("chars_per_word: NA y cadenas sin palabras", {
  x <- c("", NA_character_, "uno")
  expect_equal(chars_per_word(x), c(NA_real_, NA_real_, 3))
  expect_equal(chars_per_word(x, na_as_zero = TRUE), c(0, 0, 3))
})

test_that("chars_per_word: vectoriza", {
  v <- c("aa bb", "c c c")
  # (2+2)/2 = 2 ; (1+1+1)/3 = 1
  expect_equal(chars_per_word(v), c(2, 1))
})
