test_that(".resolve_key mapea nombres cortos a opciones completas y falla si no existe", {
  expect_identical(.resolve_key("valid_letters"), "syll.es.valid_letters")
  expect_identical(.resolve_key("v_fuerte"),      "syll.es.v_fuerte")
  expect_identical(.resolve_key("v_debil"),        "syll.es.v_debil")
  expect_identical(.resolve_key("v_tilde"),        "syll.es.v_tilde")
  expect_identical(.resolve_key("clusters_lr"),    "syll.es.clusters_lr")
  expect_identical(.resolve_key("cache_enabled"),  "syll.cache_enabled")
  expect_identical(.resolve_key("lang"),           "syll.lang")          # NUEVO

  # Si ya viene con prefijo, lo respeta
  expect_identical(.resolve_key("syll.es.v_tilde"), "syll.es.v_tilde")
  expect_identical(.resolve_key("syll.lang"),        "syll.lang")         # NUEVO

  expect_error(.resolve_key("desconocida"),
               regexp = "Opción desconocida", fixed = FALSE)
})

test_that("syll_config devuelve defaults mezclados con opciones actuales", {
  withr::local_options(.syll_default_options())
  withr::local_options(list("syll.cache_enabled" = FALSE))

  cfg <- syll_config()

  # Debe traer todas las claves sin prefijo, incluyendo lang
  expect_true(all(c("valid_letters", "v_fuerte", "v_debil", "v_tilde",
                    "clusters_lr", "cache_enabled", "lang") %in% names(cfg)))  # NUEVO: lang

  expect_identical(cfg$cache_enabled, FALSE)
  expect_identical(cfg$lang, "es")                                             # NUEVO

  dflt <- .syll_default_options()
  expect_identical(cfg$valid_letters, dflt[["syll.es.valid_letters"]])
  expect_identical(cfg$v_fuerte,      dflt[["syll.es.v_fuerte"]])
  expect_identical(cfg$v_debil,       dflt[["syll.es.v_debil"]])
  expect_identical(cfg$v_tilde,       dflt[["syll.es.v_tilde"]])
  expect_identical(cfg$clusters_lr,   dflt[["syll.es.clusters_lr"]])
})

test_that("syll_get_option prioriza opción seteada; si no, usa default explícito o default interno", {
  withr::local_options(.syll_default_options())
  dflt <- .syll_default_options()

  expect_identical(syll_get_option("valid_letters"), dflt[["syll.es.valid_letters"]])
  expect_identical(syll_get_option("lang"), "es")                              # NUEVO

  expect_identical(syll_get_option("syll.es.inexistente", default = 123L), 123L)

  withr::local_options(list("syll.cache_enabled" = FALSE))
  expect_identical(syll_get_option("cache_enabled"), FALSE)
})

# NUEVO -----------------------------------------------------------------------
test_that("syll_get_option('lang') devuelve 'es' por defecto", {
  withr::local_options(.syll_default_options())
  expect_identical(syll_get_option("lang"), "es")
})

test_that("syll_set_options(lang = 'ca') cambia el idioma global", {
  withr::local_options(.syll_default_options())
  withr::defer(syll_reset_options("all"))

  syll_set_options(lang = "ca")
  expect_identical(syll_get_option("lang"), "ca")
  expect_identical(getOption("syll.lang"), "ca")
})

test_that("syll_set_options(lang) valida que sea 'es' o 'ca'", {
  withr::local_options(.syll_default_options())

  expect_error(syll_set_options(lang = "fr"), regexp = "es.*ca|ca.*es", fixed = FALSE)
  expect_error(syll_set_options(lang = 123),  regexp = "character", fixed = FALSE)
  expect_error(syll_set_options(lang = c("es", "ca")), regexp = "character", fixed = FALSE)
})

test_that("syll_reset_options('all') restaura lang a 'es'", {
  withr::local_options(.syll_default_options())
  options("syll.lang" = "ca")

  syll_reset_options("all")
  expect_identical(syll_get_option("lang"), "es")
})

test_that("syll_reset_options puede restaurar solo lang", {
  withr::local_options(.syll_default_options())
  options("syll.lang" = "ca")
  options("syll.cache_enabled" = FALSE)

  syll_reset_options("lang")

  expect_identical(syll_get_option("lang"), "es")
  # cache_enabled no debe haber sido tocado
  expect_identical(getOption("syll.cache_enabled"), FALSE)
})
# FIN NUEVO -------------------------------------------------------------------

test_that("syll_set_options valida tipos y contenido; devuelve opciones previas", {
  withr::local_options(.syll_default_options())
  withr::defer(syll_reset_options("all"))

  letters_vec <- strsplit(.valid_letters, "", fixed = TRUE)[[1]]
  subset_letters <- paste0(letters_vec[seq_len(min(10L, length(letters_vec)))], collapse = "")

  old <- syll_set_options(
    valid_letters = subset_letters,
    v_fuerte      = c("a","á","e","é","o","ó"),
    v_debil       = c("i","u","ï","ü"),
    v_tilde       = c("á","é","í","ó","ú"),
    clusters_lr   = c("pl","tr","br"),
    cache_enabled = TRUE
  )
  expect_type(old, "list")

  expect_identical(getOption("syll.es.valid_letters"), subset_letters)
  expect_identical(getOption("syll.es.v_fuerte"),      c("a","á","e","é","o","ó"))
  expect_identical(getOption("syll.es.v_debil"),        c("i","u","ï","ü"))
  expect_identical(getOption("syll.es.v_tilde"),        c("á","é","í","ó","ú"))
  expect_identical(getOption("syll.es.clusters_lr"),    c("pl","tr","br"))
  expect_identical(getOption("syll.cache_enabled"),  TRUE)
})

test_that("syll_set_options verifica errores de validación (tipos / contenido)", {
  withr::local_options(.syll_default_options())

  expect_error(syll_set_options(valid_letters = c("ab","cd")),
               regexp = "debe ser character\\(1\\)", fixed = FALSE)
  expect_error(syll_set_options(valid_letters = "ABC"),
               regexp = "símbolos no permitidos", fixed = FALSE)
  expect_error(syll_set_options(v_fuerte = 1:3),
               regexp = "vector de character", fixed = FALSE)
  expect_error(syll_set_options(v_debil = c("a", "1")),
               regexp = "símbolos no permitidos", fixed = FALSE)
  expect_error(syll_set_options(clusters_lr = c("pl","tre")),
               regexp = "bigramas", fixed = FALSE)
  expect_error(syll_set_options(clusters_lr = c("pá", "br")),
               regexp = "no debe contener vocales", fixed = FALSE)
  expect_error(syll_set_options(clusters_lr = c("p1", "br")),
               regexp = "alfabeto válido", fixed = FALSE)
  expect_error(syll_set_options(clusters_lr = c("bp", "br")),
               regexp = "debe terminar en 'l' o 'r'", fixed = FALSE)
  expect_error(syll_set_options(cache_enabled = c(TRUE, FALSE)),
               regexp = "logical\\(1\\)", fixed = FALSE)
  expect_error(syll_set_options(cache_enabled = NA),
               regexp = "no NA", fixed = FALSE)
})

test_that("syll_reset_options('all') restaura todos los valores por defecto", {
  withr::local_options(.syll_default_options())

  options("syll.cache_enabled" = FALSE)
  options("syll.es.v_debil" = c("i"))

  res <- syll_reset_options("all")
  expect_type(res, "list")

  dflt <- .syll_default_options()
  expect_identical(getOption("syll.cache_enabled"), dflt[["syll.cache_enabled"]])
  expect_identical(getOption("syll.es.v_debil"),       dflt[["syll.es.v_debil"]])
})

test_that("syll_reset_options(vector) restaura solo las claves indicadas", {
  withr::local_options(.syll_default_options())

  options("syll.cache_enabled" = FALSE)
  options("syll.es.v_tilde" = c("á"))

  syll_reset_options(c("v_tilde", "syll.cache_enabled"))

  dflt <- .syll_default_options()
  expect_identical(getOption("syll.es.v_tilde"),       dflt[["syll.es.v_tilde"]])
  expect_identical(getOption("syll.cache_enabled"), dflt[["syll.cache_enabled"]])
})

test_that("helpers internos de validación: .ensure_subset_letters y .ensure_clusters", {
  withr::local_options(.syll_default_options())

  letters_vec <- strsplit(.valid_letters, "", fixed = TRUE)[[1]]

  expect_silent(.ensure_subset_letters(paste0(letters_vec[1:5], collapse = ""), "valid_letters"))
  expect_silent(.ensure_subset_letters(letters_vec[1:3], "valid_letters"))
  expect_error(.ensure_subset_letters("ABC", "valid_letters"),
               regexp = "símbolos no permitidos", fixed = FALSE)

  expect_silent(.ensure_clusters(c("pl","br","tr")))
  expect_error(.ensure_clusters(c("pá","br")),
               regexp = "no debe contener vocales", fixed = FALSE)
  expect_error(.ensure_clusters(c("tre","br")),
               regexp = "bigramas", fixed = FALSE)
  expect_error(.ensure_clusters(c("bp","br")),
               regexp = "debe terminar en 'l' o 'r'", fixed = FALSE)
  expect_error(.ensure_clusters(c("p1","br")),
               regexp = "alfabeto válido", fixed = FALSE)
})

test_that("syll_set_options acepta claves cortas y completas indistintamente", {
  withr::local_options(.syll_default_options())
  syll_set_options(cache_enabled = FALSE, "syll.es.v_debil" = c("i","u","ï","ü"))
  expect_identical(getOption("syll.cache_enabled"), FALSE)
  expect_identical(getOption("syll.es.v_debil"), c("i","u","ï","ü"))
})
