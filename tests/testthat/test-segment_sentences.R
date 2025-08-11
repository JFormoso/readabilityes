# tests/testthat/test-segment_sentences.R

test_that("valida tipo de entrada", {
  expect_error(segment_sentences(1:3), "`text` debe ser un vector de caracteres")
})

test_that("segmenta oraciones básicas con ., ?, ! (manteniendo delimitadores por defecto)", {
  txt <- "Hola. ¿Cómo estás? Todo bien, ¡gracias!"
  out <- segment_sentences(txt)

  expect_type(out, "list")
  expect_length(out, 1L)
  s <- out[[1]]

  expect_equal(length(s), 3L)
  expect_equal(s[1], "Hola.")
  expect_equal(s[2], "¿Cómo estás?")
  expect_equal(s[3], "Todo bien, ¡gracias!")
})

test_that("opción keep_delim = FALSE no conserva los signos finales", {
  txt <- "Hola. ¿Cómo estás? Todo bien!"
  out <- segment_sentences(txt, keep_delim = FALSE)[[1]]
  expect_equal(out, c("Hola", "¿Cómo estás", "Todo bien"))
})

test_that("protege abreviaturas comunes (Sr., Dra., etc.)", {
  txt <- "El Sr. Gómez llegó. La Dra. Pérez también."
  out <- segment_sentences(txt)[[1]]
  expect_equal(out,
               c("El Sr. Gómez llegó.", "La Dra. Pérez también."))
})

test_that("protege iniciales (J. R. R.)", {
  txt <- "J. R. R. Tolkien escribió mucho. Etc. fin."
  out <- segment_sentences(txt)[[1]]
  expect_equal(out, c("J. R. R. Tolkien escribió mucho.", "Etc. fin."))
})

test_that("protege números con separador decimal (punto y coma) y elipsis", {
  txt <- "Llegó a las 10.30... luego siguió. En total, 3,5 horas."
  out <- segment_sentences(txt)[[1]]
  expect_equal(out,
               c("Llegó a las 10.30... luego siguió.", "En total, 3,5 horas."))
})

test_that("maneja referencias editoriales (págs., cap.) y signos invertidos", {
  txt <- "Ver págs. 12-14, cap. 3. Además, ver apéndices. ¿De acuerdo?"
  out <- segment_sentences(txt)[[1]]
  expect_equal(out,
               c("Ver págs. 12-14, cap. 3.",
                 "Además, ver apéndices.",
                 "¿De acuerdo?"))
})

test_that("acepta abreviaturas adicionales vía extra_abbr (case-insensitive)", {
  txt <- "Coord. Gral. del proyecto. Reunión mañana."
  out <- segment_sentences(txt, extra_abbr = c("Coord", "gral"))[[1]]
  expect_equal(out,
               c("Coord. Gral. del proyecto.", "Reunión mañana."))
})

test_that("normalize_spacing = TRUE compacta y recorta espacios; FALSE preserva internos", {
  txt <- "Hola   mundo.   Adiós!"
  out_true  <- segment_sentences(txt, normalize_spacing = TRUE)[[1]]
  out_false <- segment_sentences(txt, normalize_spacing = FALSE)[[1]]

  # Con normalización: espacios internos compactados en la primera oración
  expect_equal(out_true[1], "Hola mundo.")
  # Sin normalización: espacios internos preservados en la primera oración
  expect_equal(out_false[1], "Hola   mundo.")

  # Después del corte, no deben quedar espacios al inicio
  expect_match(out_true[2], "^[^ ]", perl = TRUE)
  expect_match(out_false[2], "^[^ ]", perl = TRUE)
})

test_that("maneja NA y cadenas vacías devolviendo character(0) por elemento", {
  txt <- c(NA_character_, "")
  out <- segment_sentences(txt)

  expect_length(out, 2L)
  expect_equal(out[[1]], character(0))
  expect_equal(out[[2]], character(0))
})

test_that("vectoriza sobre varios elementos", {
  txt <- c("Hola. Chau.", "¿Vas? ¡Sí!")
  out <- segment_sentences(txt)

  expect_length(out, 2L)
  expect_equal(out[[1]], c("Hola.", "Chau."))
  expect_equal(out[[2]], c("¿Vas?", "¡Sí!"))
})
