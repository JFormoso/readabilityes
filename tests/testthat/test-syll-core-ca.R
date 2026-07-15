# tests/testthat/test-syll_core-ca.R
#
# Tests de silabificación para catalán.
# Organizado por niveles de complejidad creciente, siguiendo el orden
# de implementación definido en el plan.
#
# Convención: syll_ca() es un helper local que fija lang = "ca" y usa
# el punto medio (·) como separador para mayor legibilidad.

syll_ca <- function(word) syll_hyphenate(word, lang = "ca", hyphen = "\u00b7")


# Nivel 1: Reglas básicas V/C

test_that("[CA] VCV: consonante única pasa a la sílaba siguiente", {
  expect_equal(syll_ca("amic"), "a\u00b7mic")
  expect_equal(syll_ca("casa"), "ca\u00b7sa")
})

test_that("[CA] VCCV: dos consonantes se separan (VC·CV)", {
  expect_equal(syll_ca("carta"), "car\u00b7ta")
})


# Nivél 2: Dígrafos (bloques inseparables)

test_that("[CA] Dígrafo LL no se separa", {
  expect_equal(syll_ca("cavall"), "ca\u00b7vall")
})

test_that("[CA] Dígrafo NY no se separa", {
  expect_equal(syll_ca("penya"), "pe\u00b7nya")
})

test_that("[CA] Dígrafo RR no se separa", {
  expect_equal(syll_ca("terra"), "ter\u00b7ra")
})

test_that("[CA] Dígrafo SS no se separa", {
  expect_equal(syll_ca("passar"), "pas\u00b7sar")
})

test_that("[CA] Dígrafo GU no se separa ante e/i (u muda)", {
  expect_equal(syll_ca("guerra"), "guer\u00b7ra")
  expect_equal(syll_ca("guia"),   "gui\u00b7a")
})

test_that("[CA] Dígrafo QU no se separa ante e/i (u muda)", {
  expect_equal(syll_ca("quedar"), "que\u00b7dar")
})


# Nivél 3: Grupos consonánticos inseparables

test_that("[CA] Grupos bl, br, cl, cr, dr, fl, fr, gl, gr, pl, pr, tr no se separan", {
  expect_equal(syll_ca("agradar"),  "a\u00b7gra\u00b7dar")
  expect_equal(syll_ca("problema"), "pro\u00b7ble\u00b7ma")
})

test_that("[CA] VCCCV: tres consonantes → VC·CCV si las dos últimas son grupo inseparable", {
  expect_equal(syll_ca("sempre"), "sem\u00b7pre")
})

test_that("[CA] Reglas consonánticas estándar resuelven casos tipo 'impossible'", {
  expect_equal(syll_ca("impossible"), "im\u00b7pos\u00b7si\u00b7ble")
})


# Nivél 4: Diptongos y triptongos

test_that("[CA] Diptongo vocal + i/u no acentuada no se separa", {
  expect_equal(syll_ca("aire"),  "ai\u00b7re")
  expect_equal(syll_ca("caure"), "cau\u00b7re")
})

test_that("[CA] Triptongo i/u + vocal fuerte + i/u (átonas) no se separa", {
  expect_equal(syll_ca("creieu"), "cre\u00b7ieu")
})


# Nivél 5: Hiatos

test_that("[CA] i/u acentuada fuerza hiato con vocal adyacente", {
  expect_equal(syll_ca("pa\u00eds"), "pa\u00b7\u00eds")   # país
})

test_that("[CA] Dos vocales fuertes (a, e, o) forman hiato", {
  expect_equal(syll_ca("teatre"), "te\u00b7a\u00b7tre")
})

# Nivél 6: Preprocesamiento: guión y apóstrofe


test_that("[CA] Guión: pronombre unido al verbo forma sílaba adicional", {
  expect_equal(syll_ca("agradar-me"), "a\u00b7gra\u00b7dar\u00b7me")
})

test_that("[CA] Apóstrofe inicial: artículo se fusiona con la primera sílaba", {
  expect_equal(syll_ca("l'Anna"), "l'an\u00b7na")
  expect_equal(syll_ca("d'anar"), "d'a\u00b7nar")
})

test_that("[CA] Apóstrofe final: pronombre se fusiona con la última sílaba", {
  expect_equal(syll_ca("perdre'l"),  "per\u00b7drel")
  expect_equal(syll_ca("anar-se'n"), "a\u00b7nar\u00b7sen")
})

