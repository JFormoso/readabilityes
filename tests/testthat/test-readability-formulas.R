# Valores de referencia: mismo texto que usamos para verificar a mano
# "Este es un texto de prueba." -> W=6, S=1, Sy=9, C=3.5 (21 letras/6 palabras)

test_that("[formulas] szigriszt_pazos calcula correctamente", {
  expect_equal(.formula_szigriszt_pazos(W = 6, S = 1, Sy = 9), 107.385, tolerance = 1e-6)
})

test_that("[formulas] szigriszt_pazos propaga NA", {
  expect_true(is.na(.formula_szigriszt_pazos(W = NA, S = 1, Sy = 9)))
  expect_true(is.na(.formula_szigriszt_pazos(W = 6, S = NA, Sy = 9)))
  expect_true(is.na(.formula_szigriszt_pazos(W = 6, S = 1, Sy = NA)))
})

test_that("[formulas] gutierrez_de_polini usa letras, no sílabas", {
  # Con C=3.5 (letras/palabra): 95.2 - 9.7*3.5 - 0.35*6 = 59.15
  expect_equal(.formula_gutierrez_de_polini(W = 6, S = 1, C = 3.5), 59.15, tolerance = 1e-6)
})

test_that("[formulas] fernandez_huerta calcula correctamente", {
  expect_equal(.formula_fernandez_huerta(W = 6, S = 1, Sy = 9), 99.84, tolerance = 1e-6)
})

test_that("[formulas] crawford redondea P\u0304 al décimo antes de aplicar la fórmula", {
  # P\u0304 = round((1/6)*100, 1) = 16.7 ; S\u0304 = (9/6)*100 = 150
  expect_equal(.formula_crawford(W = 6, S = 1, Sy = 9), 0.5195, tolerance = 1e-4)
})

test_that("[formulas] mu calcula correctamente y devuelve NA si var_len es 0 o NA", {
  expect_equal(.formula_mu(n = 6, mean_len = 3.5, var_len = 3.1), 135.4839, tolerance = 1e-3)
  expect_true(is.na(.formula_mu(n = 3, mean_len = 4, var_len = 0)))
  expect_true(is.na(.formula_mu(n = 1, mean_len = NA, var_len = NA)))
})

test_that("[formulas] .mu_stats maneja textos de 0 o 1 palabra sin romperse", {
  st <- .mu_stats(c("", "sola", "dos palabras"), lang = "es")
  expect_equal(st$n, c(0L, 1L, 2L))
  expect_true(is.na(st$mean_len[1]))
  expect_true(is.na(st$var_len[1]))
  expect_true(is.na(st$mean_len[2]))  # una sola palabra: var no definida
  expect_true(is.na(st$var_len[2]))
  expect_false(is.na(st$var_len[3]))
})

test_that("[formulas] .inflesz_category respeta los límites exactos de la tabla oficial", {
  scores <- c(39.9, 40, 55, 55.1, 65, 65.1, 80, 80.1)
  expect_equal(
    .inflesz_category(scores),
    c("Muy dif\u00edcil", "Algo dif\u00edcil", "Algo dif\u00edcil", "Normal",
      "Normal", "Bastante f\u00e1cil", "Bastante f\u00e1cil", "Muy f\u00e1cil")
  )
})

test_that("[formulas] .mu_category respeta los límites exactos de la tabla oficial", {
  scores <- c(30, 31, 50, 51, 60, 61, 70, 71, 80, 81, 90, 91)
  expect_equal(
    .mu_category(scores),
    c("Muy dif\u00edcil", "Dif\u00edcil", "Dif\u00edcil", "Un poco dif\u00edcil",
      "Un poco dif\u00edcil", "Adecuado", "Adecuado", "Un poco f\u00e1cil",
      "Un poco f\u00e1cil", "F\u00e1cil", "F\u00e1cil", "Muy F\u00e1cil")
  )
})

test_that("[registro] cada entrada tiene la estructura esperada", {
  for (nm in names(.readability_formula_registry)) {
    entry <- .readability_formula_registry[[nm]]
    expect_true(is.function(entry$fn), info = nm)
    expect_type(entry$inputs, "character")
    expect_type(entry$langs, "character")
    expect_type(entry$reference, "character")
    expect_true(is.null(entry$category_fn) || is.function(entry$category_fn), info = nm)
  }
})

test_that("[registro] mu depende de n/mean_len/var_len, el resto de W/S/Sy o C", {
  expect_setequal(.readability_formula_registry$mu$inputs, c("n", "mean_len", "var_len"))
  expect_true(all(c("W", "S") %in% .readability_formula_registry$szigriszt_pazos$inputs))
  expect_true("C" %in% .readability_formula_registry$gutierrez_de_polini$inputs)
})

test_that("[readability_reference] devuelve el placeholder de un índice puntual", {
  ref <- readability_reference("crawford")
  expect_type(ref, "character")
  expect_length(ref, 1L)
})

test_that("[readability_reference] sin argumentos devuelve todas, nombradas", {
  refs <- readability_reference()
  expect_type(refs, "character")
  expect_setequal(names(refs), names(.readability_formula_registry))
})

test_that("[readability_reference] índice desconocido tira error explícito", {
  expect_error(readability_reference("no_existe"), regexp = "desconocido")
})

test_that("[formulas] .mu_stats maneja textos de 0 o 1 palabra sin romperse", {
  expect_warning(
    st <- .mu_stats(c("", "sola", "dos palabras"), lang = "es"),
    regexp = "Mu no puede calcularse"
  )
  expect_equal(st$n, c(0L, 1L, 2L))
  expect_true(is.na(st$mean_len[1]))
  expect_true(is.na(st$var_len[1]))
  expect_true(is.na(st$mean_len[2]))
  expect_true(is.na(st$var_len[2]))
  expect_false(is.na(st$var_len[3]))
})


