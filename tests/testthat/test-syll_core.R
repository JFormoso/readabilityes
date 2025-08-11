# tests/testthat/test-syll_core.R

test_that("syll_split: casos básicos y clusters con l/r", {
  expect_equal(syll_split("canción"),   c("can", "ción"))
  expect_equal(syll_split("pingüino"),  c("pin", "güi", "no"))
  expect_equal(syll_split("transporte"), c("trans", "por", "te"))  # VCCCV con 'nsp' => trans-porte
  expect_equal(syll_split("abrazo"),    c("a", "bra", "zo"))       # VCCV con 'br' (no se separa)
  expect_equal(syll_split("aplicar"),   c("a", "pli", "car"))      # 'pl' al onset siguiente
})

test_that("syll_split: diptongos y hiatos forzados por tilde (í/ú)", {
  expect_equal(syll_split("aire"),   c("ai", "re"))      # fuerte + débil => diptongo
  expect_equal(syll_split("ciudad"), c("ciu", "dad"))    # débil + fuerte => diptongo
  expect_equal(syll_split("frío"),   c("frí", "o"))      # í fuerza hiato
  expect_equal(syll_split("país"),   c("pa", "ís"))      # í fuerza hiato
})

test_that("syll_split: palabras sin vocales o entradas vacías/NA", {
  # Sin vocales => devuelve la palabra (regla del núcleo)
  expect_equal(syll_split("psst"), "psst")

  # Vacías/NA => NA_character_
  expect_true(is.na(syll_split(NA_character_)))
  expect_true(is.na(syll_split("")))
})

test_that("syll_count coincide con la longitud de syll_split", {
  palabras <- c("canción", "programa", "ciudad", "frío", "psst")
  splits   <- lapply(palabras, syll_split)

  esperados <- vapply(splits, length, integer(1), USE.NAMES = FALSE)
  obtenidos <- vapply(palabras, syll_count, integer(1), USE.NAMES = FALSE)

  expect_equal(obtenidos, esperados)
})

test_that("syll_hyphenate une con separador por defecto y personalizado", {
  expect_equal(syll_hyphenate("silabificación"),
               paste(syll_split("silabificación"), collapse = "-"))
  expect_equal(syll_hyphenate("silabificación", hyphen = "·"),
               paste(syll_split("silabificación"), collapse = "·"))
})

test_that("vectorización simple: aplica a un vector mediante vapply/lapply externamente", {
  v <- c("canción", "aire", "abrazo")
  out <- lapply(v, syll_split)
  expect_equal(out[[1]], c("can", "ción"))
  expect_equal(out[[2]], c("ai", "re"))
  expect_equal(out[[3]], c("a", "bra", "zo"))
})

test_that("reglas de corte VCCV vs V-CCV según clúster lr", {
  # VCCV sin clúster lr -> VC-CV (ej: 'actor' => ac-tor)
  expect_equal(syll_split("actor"), c("ac", "tor"))

  # VCCV con clúster lr -> V-CCV (ej: 'abril' => a-bril)
  expect_equal(syll_split("abril"), c("a", "bril"))
})

test_that("regla VCCCV: si las dos últimas forman clúster lr => VC-CCV; si no => VCC-CV", {
  # 'inscrito' entre núcleos: nscr (tomamos 'scr', últimas dos 'cr' forman clúster) => ins-cri-to
  expect_equal(syll_split("inscrito"), c("ins", "cri", "to"))

  # 'transporte' ya probado: nsp (últimas dos 'sp' no forman clúster) => trans-porte
  expect_equal(syll_split("transporte"), c("trans", "por", "te"))
})

test_that("salida es determinista (mismas entradas => mismas salidas)", {
  palabras <- c("canción", "pingüino", "ciudad", "frío", "actor", "abril")
  out1 <- lapply(palabras, syll_split)
  out2 <- lapply(palabras, syll_split)
  expect_equal(out1, out2)
})

# ----- Test opcional: interacción con caché (si existen helpers y opción de set) -----

test_that("caché: cuando está habilitado guarda y reutiliza resultados", {
  skip_if_not(exists("syll_get_option"))
  skip_if_not(exists("syll_set_option"))
  skip_if_not(exists(".cache_get",  where = asNamespace("readabilityes")))
  skip_if_not(exists(".cache_clear", where = asNamespace("readabilityes")))

  # Guardar y restaurar opción
  old_opt <- syll_get_option("cache_enabled")
  withr::defer(syll_set_option(cache_enabled = old_opt), teardown_env())

  syll_set_option(cache_enabled = TRUE)
  readabilityes:::.cache_clear()
  withr::defer(readabilityes:::.cache_clear(), teardown_env())

  w <- "cancion"  # sin tilde para evitar normalizaciones
  expect_null(readabilityes:::.cache_get(w))

  # Primera llamada: debe poblar el caché
  s1 <- syll_split(w)
  in_cache <- readabilityes:::.cache_get(w)
  expect_equal(in_cache, s1)

  # Segunda llamada: mismo resultado (potencialmente desde caché)
  s2 <- syll_split(w)
  expect_equal(s1, s2)
})
