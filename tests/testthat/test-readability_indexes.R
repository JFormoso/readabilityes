# tests/testthat/test-readability_indexes.R

test_that("szigriszt_pazos calcula según la fórmula con conteos simples", {
  # Texto trivial: "a." -> W=1, S=1, Sy=1
  txt <- "a."
  esperado <- 206.835 - 62.3 * (1/1) - (1/1)  # 143.535
  obt <- szigriszt_pazos(txt)
  expect_equal(obt, esperado, tolerance = 1e-10)

  # Vectoriza y propaga NA
  vtxt <- c("a.", NA_character_)
  obt2 <- szigriszt_pazos(vtxt)
  expect_equal(length(obt2), 2L)
  expect_true(is.na(obt2[2]))
})

test_that("gutierrez_de_polini calcula según la fórmula con conteos simples", {
  # "a." -> W=1, S=1, Sy=1
  txt <- "a."
  esperado <- 95.2 - 9.7 * (1/1) - 0.35 * (1/1)  # 85.15
  obt <- gutierrez_de_polini(txt)
  expect_equal(obt, esperado, tolerance = 1e-10)
})


test_that("inflesz_category asigna categorías correctas (umbrales incluidos)", {
  # Umbrales:
  # <40 -> Difícil
  # 40..55 -> Algo difícil
  # >55..65 -> Normal
  # >65..80 -> Fácil
  # >80 -> Muy fácil
  sc <- c(NA, 39.99, 40, 55, 55.0001, 65, 65.0001, 80, 81)
  cat <- inflesz_category(sc)
  expect_equal(
    cat,
    c(NA_character_,
      "Dif\u00edcil",
      "Algo dif\u00edcil",
      "Algo dif\u00edcil",
      "Normal",
      "Normal",
      "F\u00e1cil",
      "F\u00e1cil",
      "Muy f\u00e1cil")
  )
})

test_that("inflesz devuelve data.frame con score y category (ordenada) y coincide con szigriszt_pazos", {
  v <- c("a.", "a. a.", NA_character_)  # 1 oración/1 palabra y 2 oraciones/2 palabras; NA
  out <- inflesz(v)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("score", "category"))
  expect_type(out$score, "double")
  expect_s3_class(out$category, "ordered")

  # Niveles correctos y en orden de dificultad
  expect_equal(
    levels(out$category),
    c("Dif\u00edcil", "Algo dif\u00edcil", "Normal", "F\u00e1cil", "Muy f\u00e1cil")
  )

  # El puntaje debe coincidir con szigriszt_pazos()
  expect_equal(out$score, szigriszt_pazos(v), tolerance = 1e-12)

  # NA se propaga
  expect_true(is.na(out$score[3]))
  expect_true(is.na(out$category[3]))
})

test_that("inputs sin palabras/oraciones no rompen: se usan pmax(...) y se evitan NaN", {
  # Cadena vacía -> W=0, S=0, Sy=0; fórmulas usan pmax(W,S,1) => finite
  empty <- ""
  sp <- szigriszt_pazos(empty)
  gp <- gutierrez_de_polini(empty)

  expect_true(is.finite(sp))
  expect_true(is.finite(gp))
})
